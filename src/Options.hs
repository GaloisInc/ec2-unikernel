module Options(
         Options
       , defaultOptions
       , optAwsAccessKey
       , optAwsSecretKey
       , optS3Bucket
       , optS3Zone
       , optAwsRegion
       , optTargetKey
       , optImageName
       , optKernel
       , optKernelArgs
       , optRamdisks
       )
 where

import Control.Lens(Lens', lens)
import Data.String(fromString)
import Data.Text(Text)
import Network.AWS(AccessKey, SecretKey, Region(..))
import Network.AWS.S3.Types(BucketName, ObjectKey)

data Options = Options
  { _optAwsAccessKey :: AccessKey
  , _optAwsSecretKey :: SecretKey
  , _optS3Bucket     :: BucketName
  , _optS3Zone       :: Text
  , _optTargetKey    :: ObjectKey
  , _optAwsRegion    :: Region
  , _optKernel       :: FilePath
  , _optKernelArgs   :: String
  , _optRamdisks     :: [FilePath]
  , _optImageName    :: Text
  }

defaultOptions :: Options
defaultOptions = Options
  { _optAwsAccessKey = fromString ""
  , _optAwsSecretKey = fromString ""
  , _optS3Bucket     = fromString "unikernels"
  , _optS3Zone       = fromString "us-west-2a"
  , _optTargetKey    = fromString "badfile/,:"
  , _optAwsRegion    = Oregon
  , _optKernel       = "kernel"
  , _optKernelArgs   = ""
  , _optRamdisks     = []
  , _optImageName    = fromString ""
  }

-- We're explicitly writing these instances because some combination of
-- Amazonka, lens, and Template Haskell explode when we try to build this
-- under GHC 7.8.4.

optAwsAccessKey :: Lens' Options AccessKey
optAwsAccessKey = lens _optAwsAccessKey (\ x v -> x{ _optAwsAccessKey = v })

optAwsSecretKey :: Lens' Options SecretKey
optAwsSecretKey = lens _optAwsSecretKey (\ x v -> x{ _optAwsSecretKey = v })

optS3Bucket :: Lens' Options BucketName
optS3Bucket = lens _optS3Bucket (\ x v -> x{ _optS3Bucket = v })

optS3Zone :: Lens' Options Text
optS3Zone = lens _optS3Zone (\ x v -> x{ _optS3Zone = v })

optTargetKey :: Lens' Options ObjectKey
optTargetKey = lens _optTargetKey (\ x v -> x{ _optTargetKey = v })

optAwsRegion :: Lens' Options Region
optAwsRegion = lens _optAwsRegion (\ x v -> x{ _optAwsRegion = v })

optKernel :: Lens' Options FilePath
optKernel = lens _optKernel (\ x v -> x{ _optKernel = v })

optKernelArgs :: Lens' Options String
optKernelArgs = lens _optKernelArgs (\ x v -> x{ _optKernelArgs = v })

optRamdisks :: Lens' Options [FilePath]
optRamdisks = lens _optRamdisks (\ x v -> x{ _optRamdisks = v })

optImageName :: Lens' Options Text
optImageName = lens _optImageName (\ x v -> x{ _optImageName = v })

