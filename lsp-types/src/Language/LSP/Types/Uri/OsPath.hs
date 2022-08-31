{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if MIN_VERSION_filepath(1,4,100)
#define OS_PATH 1
#endif

module Language.LSP.Types.Uri.OsPath
  (
#ifdef OS_PATH
    osPathToNormalizedFilePath
  , normalizedFilePathToOsPath
#endif
  ) where

#ifdef OS_PATH

import           Control.DeepSeq        (NFData, force)
import           Control.Exception      hiding (try)
import           Control.Monad.Catch
import           Language.LSP.Types.Uri
import           System.IO.Unsafe       (unsafePerformIO)
import           System.OsPath

{-|
Constructs 'NormalizedFilePath' from 'OsPath'. Throws 'IOException' if the conversion fails.
-}
osPathToNormalizedFilePath :: MonadThrow m => OsPath -> m NormalizedFilePath
osPathToNormalizedFilePath = fmap toNormalizedFilePath . unsafePerformIO' . decodeFS

{-|
Extracts 'OsPath' from 'NormalizedFilePath'. Throws 'IOException' if the conversion fails.
-}
normalizedFilePathToOsPath :: MonadThrow m => NormalizedFilePath -> m OsPath
normalizedFilePathToOsPath = unsafePerformIO' . encodeFS . fromNormalizedFilePath

unsafePerformIO' :: (MonadThrow m, NFData a) => IO a -> m a
unsafePerformIO' action =
  case fp of
    Left (e :: SomeException) -> throwM e
    Right fp'                 -> pure fp'
  where
    fp = unsafePerformIO . try $ do
      x <- action
      evaluate . force $ x

#endif
