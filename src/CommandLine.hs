module CommandLine(getOptions)
 where

import Control.Exception(catch)
import Control.Lens(ASetter, view, set, elemOf, folded)
import Control.Monad(forM_, unless, when)
import Data.Char(isAlphaNum, toLower)
import Data.Either(isLeft)
import Data.String(IsString, fromString)
import Data.Time.Clock(UTCTime, getCurrentTime)
import Data.Time.Format(formatTime, defaultTimeLocale)
import Network.AWS(Region(..), Credentials(..), Env,
                   runAWS, runResourceT, newEnv, send)
import Network.AWS.Data(toText)
import Network.AWS.EC2.DescribeAvailabilityZones(describeAvailabilityZones,
                                                 dazrsAvailabilityZones,
                                                 dazZoneNames)
import Network.AWS.EC2.Types(azZoneName)
import Network.AWS.Types(ServiceError, Error(..))
import Options
import System.Console.GetOpt(ArgDescr(..), OptDescr(..), ArgOrder(..))
import System.Console.GetOpt(getOpt, usageInfo)
import System.Directory(doesFileExist)
import System.Environment(lookupEnv)
import System.Exit(ExitCode(ExitFailure), exitWith)
import System.FilePath(takeFileName)

type OptOrErr = Either [String] Options

addError :: OptOrErr -> String -> OptOrErr
addError (Left errs) err = Left (errs ++ [err])
addError (Right _)   err = Left [err]

addOpt :: OptOrErr -> (Options -> Options) -> OptOrErr
addOpt (Left errs) _ = Left errs
addOpt (Right o) f   = Right (f o)

validateAccessKey :: String -> OptOrErr -> OptOrErr
validateAccessKey ak opts
  | length ak /= 20           = addError opts "Access key doesn't look right."
  | any (not . isAlphaNum) ak = addError opts "Access key has weird characters."
  | otherwise                 = addOpt opts (set optAwsAccessKey (fromString ak))

validateSecretKey :: String -> OptOrErr -> OptOrErr
validateSecretKey sk opts
  | length sk /= 40           = addError opts "Secret key doesn't look right."
  | any (not . isSecKeyCh) sk = addError opts "Secret key has weird characters."
  | otherwise                 = addOpt opts (set optAwsSecretKey (fromString sk))
 where isSecKeyCh c = isAlphaNum c || (c == '/') || (c == '+')

validateS3Bucket :: String -> OptOrErr -> OptOrErr
validateS3Bucket b opts
  | any (not . isBuckCh) b = addError opts "S3 bucket has weird characters."
  | otherwise              = addOpt opts (set optS3Bucket (fromString b))
 where isBuckCh c = isAlphaNum c || (c == '-')

validateZone :: String -> OptOrErr -> OptOrErr
validateZone z opts
  | z `elem` zones = addOpt   opts (set optS3Zone (fromString z))
  | otherwise      = addError opts "Unknown S3 zone."
 where zones = ["us-west-2a"]

validateRegion :: String -> OptOrErr -> OptOrErr
validateRegion r opts =
  case lookup (map toLower r) regions of
    Nothing -> addError opts "Unknown AWS region."
    Just v  -> addOpt   opts (set optAwsRegion v)

regions :: [(String, Region)]
regions =
  [ ("ireland", Ireland), ("eu-west-1", Ireland)
  , ("frankfurt", Frankfurt), ("eu-central-1", Frankfurt)
  , ("tokyo", Tokyo), ("ap-northeast-1", Tokyo)
  , ("singapore", Singapore), ("ap-southeast-1", Singapore)
  , ("sydney", Sydney), ("ap-southeast-2", Sydney)
  , ("beijing", Beijing), ("cn-north-1", Beijing)
  , ("northvirginia", NorthVirginia), ("us-east-1", NorthVirginia)
  , ("northcalifornia", NorthCalifornia), ("us-west-1", NorthCalifornia)
  , ("oregon", Oregon), ("us-west-2", Oregon)
  , ("govcloud", GovCloud), ("us-gov-west-1", GovCloud)
  , ("govcloudfips", GovCloudFIPS), ("fips-us-gov-west-1", GovCloudFIPS)
  , ("saopaulo", SaoPaulo), ("sa-east-1", SaoPaulo)
  ]

options :: [OptDescr (OptOrErr -> OptOrErr)]
options =
  [ Option ['o'] ["aws-access-key"] (ReqArg validateAccessKey "KEY")
           "AWS access key to use"
  , Option ['w'] ["aws-secret-key"] (ReqArg validateSecretKey "VALUE")
           "AWS secret key to use"
  , Option ['b'] ["s3-bucket"] (ReqArg validateS3Bucket "BUCKET")
           "S3 bucket to upload to, temporarily."
  , Option ['z'] ["zone"] (ReqArg validateZone "ZONE")
           "S3 zone in which that bucket livees."
  , Option ['r'] ["region"] (ReqArg validateRegion "REGION")
           "S3 region to upload to."
  , Option ['a'] ["kernel-args"]
           (ReqArg (\a opts -> addOpt opts (set optKernelArgs a)) "STRING")
           "Kernel arguments to pass to the unikernel."
  ]

maybeSet :: IsString b => ASetter s s a b -> Maybe String -> s -> s
maybeSet _     Nothing  x = x
maybeSet field (Just v) x = set field (fromString v) x

getOptions :: [String] -> IO (Options, Env)
getOptions argv =
  do maccess <- lookupEnv "AWS_ACCESS_KEY"
     msecret <- lookupEnv "AWS_SECRET_KEY"
     let defaultOptions'  = maybeSet optAwsAccessKey maccess defaultOptions
         defaultOptions'' = maybeSet optAwsSecretKey msecret defaultOptions'
         (res, xs, errs)  = getOpt RequireOrder options argv
         doneOpts         = foldl (flip id) (Right defaultOptions'') res
         optErrors        = either id (const []) doneOpts
         kernelErrs       = if null xs then ["No unikernel specified!"] else []
         Right baseOpts   = doneOpts
     now                 <- getCurrentTime
     let opts             = adjustImageName
                               $ adjustTargetName now
                               $ set optKernel   (head xs)
                               $ set optRamdisks (tail xs) baseOpts
     when (isLeft doneOpts || null xs || not (null errs)) $
       fail' (optErrors ++ kernelErrs ++ errs)
     kernelOk   <- doesFileExist (view optKernel opts)
     ramdisksOk <- mapM doesFileExist (view optRamdisks opts)
     unless kernelOk $ fail' ["Unikernel not found"]
     unless (and ramdisksOk) $
       do let disks   = zip (view optRamdisks opts) ramdisksOk
              disks'  = filter (not . snd) disks
              disks'' = map fst disks'
          fail' (map (\s -> "Ramdisk "++s++" not found.") disks'')
     let akey  = view optAwsAccessKey opts
         skey  = view optAwsSecretKey opts
         creds = FromKeys akey skey
     e <- newEnv (view optAwsRegion opts) creds
     let region = toText (view optS3Zone opts)
         zoneRequest = set dazZoneNames [region] describeAvailabilityZones
     r <- catch ((runResourceT . runAWS e) (send zoneRequest))
            (\ (ServiceError se) ->
               do printInitialServiceError se
                  exitWith (ExitFailure 1))
     let z = Just (view optS3Zone opts)
     unless (elemOf (dazrsAvailabilityZones . folded . azZoneName) z r) $
       fail' ["Invalid availability zone for region."]
     return (opts, e)

printInitialServiceError :: ServiceError -> IO ()
printInitialServiceError se =
  do putStrLn "ERROR: Failed to get list of zones from Amazon. This typically"
     putStrLn "is caused by one of two problems:"
     putStrLn ""
     putStrLn "  #1: There's something wrong with your keys. Check your"
     putStrLn "      arguments, or make sure AWS_ACCESS_KEY and AWS_SECRET_KEY"
     putStrLn "      are what you want them to be."
     putStrLn "  #2: There's something wrong with your computer's clock. Run"
     putStrLn "      whatever software you have to synchronize your clock, and"
     putStrLn "      try again."
     putStrLn ""
     putStrLn "Just in case it's useful, here's the raw error:"
     putStrLn (show se)

adjustTargetName :: UTCTime -> Options -> Options
adjustTargetName now opts
  | view optTargetKey opts == view optTargetKey defaultOptions =
      let baseName = takeFileName (view optKernel opts)
          formStr  = baseName ++ "-%0C%0y%m%d-%H%M%S.raw"
          keyStr   = formatTime defaultTimeLocale formStr now
      in set optTargetKey (fromString keyStr) opts
  | otherwise = opts

adjustImageName :: Options -> Options
adjustImageName opts
  | view optImageName opts == view optImageName defaultOptions =
      set optImageName (toText (view optTargetKey opts)) opts
  | otherwise = opts

fail' :: [String] -> IO a
fail' errs =
  do forM_ errs $ \ e -> putStrLn ("ERROR: " ++ e)
     putStrLn ("\n" ++ usageInfo hdr options)
     exitWith (ExitFailure 1)
 where hdr = "Usage: ec2-unikernel [OPTION...] KERNEL [RAMDISK ...]"

