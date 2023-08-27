{- ORMOLU_DISABLE -}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if MIN_VERSION_filepath(1,4,100)
#define OS_PATH 1
#endif

module Language.LSP.Protocol.Types.Uri.OsPath
  (
#ifdef OS_PATH
    osPathToNormalizedFilePath
  , normalizedFilePathToOsPath
  , EncodingException
#endif
  ) where

#ifdef OS_PATH

import           Control.Exception      hiding (try)
import           Control.Monad.Catch
import           GHC.IO.Encoding        (getFileSystemEncoding)
import           Language.LSP.Protocol.Types.Uri
import           System.IO
import           System.IO.Unsafe       (unsafePerformIO)
import           System.OsPath
import           System.OsPath.Encoding (EncodingException)

{-|
Constructs 'NormalizedFilePath' from 'OsPath'. Throws 'EncodingException' if the conversion fails.

We store a 'Text' in 'NormalizedFilePath', which is UTF-16 or UTF-8 depending on the verion of text library.
'OsPath' may have a different encoding than 'Text', so this function may fail.
But DO NOTE THAT encoding mismatch doesn't always mean an exception will be thrown.
[Possibly your encoding simply won't throw exception on failure](https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.IO.Encoding.html#initFileSystemEncoding).
Possibly the conversion function can't find any invalid byte sequence, giving a sucessful but wrong result.
-}
osPathToNormalizedFilePath :: MonadThrow m => OsPath -> m NormalizedFilePath
osPathToNormalizedFilePath = fmap toNormalizedFilePath . liftException . decodeWith systemEnc utf16le

{-|
Extracts 'OsPath' from 'NormalizedFilePath'. Throws 'EncodingException' if the conversion fails.
-}
normalizedFilePathToOsPath :: MonadThrow m => NormalizedFilePath -> m OsPath
normalizedFilePathToOsPath = liftException . encodeWith systemEnc utf16le . fromNormalizedFilePath

liftException :: (MonadThrow m, Exception e) => Either e a -> m a
liftException (Right x)  = pure x
liftException (Left err) = throwM err

systemEnc :: TextEncoding
systemEnc = unsafePerformIO getFileSystemEncoding

#endif
