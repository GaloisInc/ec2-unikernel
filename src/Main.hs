import           CommandLine(getOptions)
import           Control.Concurrent(threadDelay)
import           Control.Lens(ASetter, set, view, elemOf, folded)
import           Control.Monad(unless, void)
import qualified Data.ByteString.Lazy as L
import           Data.Int(Int64)
import           Data.List(sortBy)
import           Data.List.NonEmpty(NonEmpty(..))
import           Data.Maybe(fromMaybe)
import           Data.Monoid((<>))
import           Data.Text(Text, unpack, isPrefixOf)
import           Network.AWS(Env, AWSRequest, Rs, runResourceT, runAWS, send)
import           Network.AWS.Data.Body(RqBody(..), toHashed, contentLength)
import           Network.AWS.Data.Text(toText)
import           Network.AWS.EC2.DescribeImages(describeImages, deseOwners, deseFilters,
                                                desrsImages)
import           Network.AWS.EC2.DescribeImportSnapshotTasks(DescribeImportSnapshotTasksResponse,
                                                             distImportTaskIds,
                                                             describeImportSnapshotTasks,
                                                             distrsResponseStatus,
                                                             distrsImportSnapshotTasks)
import           Network.AWS.EC2.ImportSnapshot(importSnapshot, isDiskContainer,
                                                isrsSnapshotTaskDetail,
                                                isrsResponseStatus,
                                                isrsImportTaskId)
import           Network.AWS.EC2.RegisterImage(registerImage,
                                               riRootDeviceName, riBlockDeviceMappings,
                                               riKernelId, riArchitecture,
                                               rirsResponseStatus, rirsImageId)
import           Network.AWS.EC2.Types(istSnapshotTaskDetail,
                                       ebsBlockDevice, ebdDeleteOnTermination,
                                       ebdVolumeSize, ebdSnapshotId,
                                       bdmEBS, blockDeviceMapping,
                                       SnapshotTaskDetail, stdStatus, stdSnapshotId,
                                       stdProgress, stdStatusMessage,
                                       snapshotDiskContainer, sdcURL, sdcFormat,
                                       iName, iImageId,
                                       filter', fValues,
                                       ArchitectureValues(..))
import           Network.AWS.S3.AbortMultipartUpload(abortMultipartUpload)
import           Network.AWS.S3.CompleteMultipartUpload(completeMultipartUpload,
                                                        cMultipartUpload,
                                                        crsResponseStatus)
import           Network.AWS.S3.CreateBucket(createBucket, cbCreateBucketConfiguration)
import           Network.AWS.S3.CreateMultipartUpload(createMultipartUpload,
                                                      cmursUploadId,
                                                      cmursResponseStatus)
import           Network.AWS.S3.ListBuckets(listBuckets, lbrsBuckets)
import           Network.AWS.S3.Types(completedMultipartUpload,
                                      cmuParts, completedPart,
                                      LocationConstraint(..),
                                      createBucketConfiguration,
                                      cbcLocationConstraint,
                                      bName)
import           Network.AWS.S3.UploadPart(uploadPart, uprsETag,
                                           uprsResponseStatus)
import           Options(Options, optS3Bucket, optTargetKey, optImageName,
                         optS3Bucket, optTargetKey, optKernel, optKernelArgs,
                         optRamdisks, optS3Bucket, optAwsRegion)
import           System.Directory(copyFile)
import           System.Environment(getArgs)
import           System.Exit(ExitCode(..), exitWith)
import           System.FilePath(takeFileName, (</>))
import           System.IO.Temp(withSystemTempDirectory)
import           System.Posix.Files(fileSize, getFileStatus)
import           System.Process(callProcess)

main :: IO ()
main =
  do args      <- getArgs
     (opts, e) <- getOptions args
     kernelId  <- findPVGrubAKI e
     makeBucket opts e
     image     <- buildDisk opts
     uploadFile opts e image
     snapshot  <- makeSnapshot opts e
     ami       <- createAMI opts e kernelId snapshot
     putStrLn (unpack ami)

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

findPVGrubAKI :: Env -> IO Text
findPVGrubAKI e =
  do res <- awsSend e findRequest
     let grubs = filter isPVGrub (view desrsImages res)
     case reverse (sortBy sortGrubs grubs) of
       [] ->
         abort "Couldn't find PV-GRUB kernel for your region."
       (kernel:_) ->
         do let kernelId        = view iImageId kernel
                Just kernelName = view iName kernel
            logm "KERNEL" ("Using kernel " ++ unpack kernelId ++ " (" ++
                           unpack kernelName ++ ")")
            return kernelId
 where
  findRequest = set deseOwners ["amazon"]
              $ set deseFilters [ set fValues ["x86_64"] (filter' "architecture")
                                , set fValues ["xen"] (filter' "hypervisor")
                                , set fValues ["paravirtual"] (filter' "virtualization-type")
                                ]
              $ describeImages
  --
  isPVGrub x =
    case view iName x of
      Nothing -> False
      Just n  -> "pv-grub-" `isPrefixOf` n
  --
  sortGrubs x y = compare (view iName x) (view iName y)

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

buildDisk :: Options -> IO FilePath
buildDisk opts =
  withSystemTempDirectory "ec2-unikernel" $ \ path ->
    do sizeb <- fileSize `fmap` getFileStatus (view optKernel opts)
       let sizem = ceiling (fromInteger (fromIntegral sizeb) / onemeg) + 1
       writeFile (path </> "menu.lst") (grubMenu opts)
       writeFile (path </> "guestfish.scr")
                 (guestFishScript (path </> "disk.raw") sizem
                                  (path </> "menu.lst") opts)
       callProcess "guestfish" ["-f", path </> "guestfish.scr"]
       let targetFile = unpack (toText (view optTargetKey opts))
       copyFile (path </> "disk.raw") targetFile
       return targetFile

grubMenu :: Options -> String
grubMenu opts = unlines
 ([ "default 0"
  , "timeout 1"
  , "title unikernel_boot"
  , "\troot (hd0,0)"
  , "\tkernel /" ++ takeFileName (view optKernel opts) ++ kernelArgs
  ] ++ map (\ f -> "\tmodule /" ++ takeFileName f) (view optRamdisks opts))
 where
  kargs = view optKernelArgs opts
  kernelArgs | null kargs = ""
             | otherwise  = " " ++ kargs

guestFishScript :: String -> Integer -> FilePath -> Options -> String
guestFishScript diskName diskSize menu opts = unlines
 ([ "disk-create " ++ diskName ++ " raw " ++ show diskSize ++ "M"
  , "add " ++ diskName
  , "run"
  , "part-disk /dev/sda mbr"
  , "mkfs ext2 /dev/sda1"
  , "mount /dev/sda1 /"
  , "mkdir /grub"
  , "sync"
  , "copy-in " ++ menu ++ " /grub/"
  , "rename /grub/" ++ takeFileName menu ++ " /grub/menu.lst"
  , "copy-in " ++ view optKernel opts ++ " /"
  ] ++ map (\ f -> "copy-in " ++ f ++ " /") (view optRamdisks opts) ++
  [ "sync"
  , "exit"
  ])

onemeg :: Double
onemeg = 1048576.0

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

amazonMinimimPartSizeInMB :: Integer
amazonMinimimPartSizeInMB = 5

amazonMinimimPartSizeInBytes :: Int64
amazonMinimimPartSizeInBytes =
  fromIntegral amazonMinimimPartSizeInMB * 1024 * 1024

statusOk :: Int
statusOk = 200

makeBucket :: Options -> Env -> IO ()
makeBucket opts e =
  do bkts <- awsSend e listBuckets
     unless (elemOf (lbrsBuckets . folded . bName) bucketName bkts) $
       do void (awsSend e createRequest)
          logm "S3" ("Created bucket " ++ unpack (toText bucketName))
 where
  bucketName    = view optS3Bucket opts
  location      = Just (LocationConstraint (view optAwsRegion opts))
  config        = set cbcLocationConstraint location createBucketConfiguration
  createRequest = set cbCreateBucketConfiguration (Just config) $
                    createBucket bucketName

uploadFile :: Options -> Env -> FilePath -> IO ()
uploadFile opts e filename =
  do rsp <- awsSend e (createMultipartUpload bucket key)
     case (view cmursUploadId rsp, view cmursResponseStatus rsp) of
       (Nothing, code) ->
         putStrLn ("ERROR: Upload initialization failed with code: "++show code)
       (Just upId, _) ->
         do bytes <- L.readFile filename
            sizeb <- fileSize `fmap` getFileStatus filename
            runUpload upId 0 (fromIntegral sizeb) bytes 1 []
 where
  bucket = view optS3Bucket  opts
  key    = view optTargetKey opts
  --
  abortUploadAndFail upId =
    do void (awsSend e (abortMultipartUpload bucket key upId))
       fail "Execution aborted (bad upload)"
  --
  runUpload upId sentSize totalSize bytes partNo completedTags
    | L.null bytes =
        case reverse completedTags of
          [] ->
            do putStrLn ("ERROR: Empty upload?")
               abortUploadAndFail upId
          (x:rest) ->
            do let parts   = x :| rest
                   baseReq = completeMultipartUpload bucket key upId
                   parts'  = set cmuParts (Just parts) completedMultipartUpload
               rsp <- awsSend e (set cMultipartUpload (Just parts') baseReq)
               if view crsResponseStatus rsp == statusOk
                  then logm "UPLOAD" "100% complete"
                  else do putStrLn "ERROR: Bad final upload code."
                          abortUploadAndFail upId
    | otherwise =
        do let (chunkBS, rest) = L.splitAt amazonMinimimPartSizeInBytes bytes
               chunk           = Hashed (toHashed chunkBS)
               sentSize'       = sentSize + contentLength chunk
           rsp <- awsSend e (uploadPart bucket key partNo upId chunk)
           case (view uprsETag rsp, view uprsResponseStatus rsp) of
             (Nothing, code) ->
               do putStrLn ("ERROR: Upload failed with code: " ++ show code)
                  putStrLn ("       Attempting abort.")
                  abortUploadAndFail upId
             (Just etag, _) ->
               do let percentDble = fromIntegral sentSize / totalSize
                      percentInt  = ceiling ((100.0 :: Double) * percentDble) :: Int
                  logm "UPLOAD" (show percentInt ++ "% complete")
                  let partNo'        = partNo + 1
                      completedTags' = completedPart partNo etag : completedTags
                  runUpload upId sentSize' totalSize rest partNo' completedTags'


-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

makeSnapshot :: Options -> Env -> IO Text
makeSnapshot opts e =
  do res <- awsSend e importRequest
     let taskId  = view isrsImportTaskId res
         errCode = view isrsResponseStatus res
         details = view isrsSnapshotTaskDetail res
     checkResponse (fromMaybe "" taskId) errCode details
 where
  url = "s3://" <> toText (view optS3Bucket opts) <> "/" <> toText (view optTargetKey opts)
  importRequest = setm isDiskContainer container importSnapshot
  container     = setm sdcFormat "RAW"
                $ setm sdcURL url
                $ snapshotDiskContainer

  loop :: Text -> IO Text
  loop taskId =
    do threadDelay 2500000
       res <- awsSend e (set distImportTaskIds [taskId] describeImportSnapshotTasks)
       checkResponse taskId (view distrsResponseStatus res) (getDetails res)

  getDetails :: DescribeImportSnapshotTasksResponse -> Maybe SnapshotTaskDetail
  getDetails rsp =
    case view distrsImportSnapshotTasks rsp of
      []    -> Nothing
      (x:_) -> view istSnapshotTaskDetail x

  checkResponse :: Text -> Int -> Maybe SnapshotTaskDetail -> IO Text
  checkResponse _      errCode Nothing =
    abort ("Could not import disk snapshot. Error code: " ++ show errCode)
  checkResponse taskId err     (Just d) | err /= 200 =
    do warn ("Weird error code from import: " ++ show err)
       checkResponse taskId 200 (Just d)
  checkResponse taskId _       (Just d) =
    processTaskDetail taskId d

  processTaskDetail :: Text -> SnapshotTaskDetail -> IO Text
  processTaskDetail taskId detail
    | view stdStatus detail == Just "completed" =
        case view stdSnapshotId detail of
          Nothing -> abort "Snapshot imported, but no id found."
          Just x  -> return x
    | otherwise =
       do logm "IMPORT" (show' (view stdProgress detail) ++ "% " ++
                         "(" ++ show' (view stdStatusMessage detail) ++ ")")
          loop taskId

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

createAMI :: Options -> Env -> Text -> Text -> IO Text
createAMI opts e kernelId snapshot =
  do res <- awsSend e registerRequest
     case (view rirsResponseStatus res, view rirsImageId res) of
       (errCode, Nothing) ->
         abort ("Could not register AMI. Error code: " ++ show errCode)
       (200, Just ami) ->
         do logm "IMPORT" "100%"
            return ami
       (err, Just ami) ->
         do warn ("Weird error code from register AMI: " ++ show err)
            return ami
 where
  ebsVol          = setm ebdDeleteOnTermination True
                  $ setm ebdVolumeSize 1 -- FIXME: Compute this
                  $ setm ebdSnapshotId snapshot
                  $ ebsBlockDevice
  blockDevMap     = setm bdmEBS ebsVol
                  $ blockDeviceMapping "/dev/sda1"
  registerRequest = setm riRootDeviceName "/dev/sda1"
                  $ set  riBlockDeviceMappings [blockDevMap]
                  $ setm riArchitecture X86_64
                  $ setm riKernelId kernelId
                  $ registerImage (view optImageName opts)

-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

abort :: String -> IO a
abort msg =
  do putStrLn ("ERROR: " ++ msg)
     exitWith (ExitFailure 1)

warn :: String -> IO ()
warn msg = putStrLn ("WARNING: " ++ msg)

setm :: ASetter s t a (Maybe b) -> b -> s -> t
setm field value obj = set field (Just value) obj

logm :: String -> String -> IO ()
logm category message = putStrLn (category ++ ": " ++ message)

awsSend :: AWSRequest r => Env -> r -> IO (Rs r)
awsSend e x = runResourceT (runAWS e (send x))

show' :: Maybe Text -> String
show' Nothing  = ""
show' (Just x) = unpack x
