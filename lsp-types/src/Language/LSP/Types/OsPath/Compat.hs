{-# LANGUAGE CPP #-}

#if MIN_VERSION_filepath(1,4,100)
#define OS_PATH 1
#endif

module Language.LSP.Types.OsPath.Compat
  ( OsPathCompat
#ifdef OS_PATH
  , module System.OsPath
#else
  , module System.FilePath
#endif
  , toShortByteString
  , fromShortByteString
  , toFilePath
  , fromFilePath
  ) where

#ifdef OS_PATH
import           System.OsPath

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import           System.OsString.Internal.Types (OsString (..),
                                                 WindowsString (..))
#else
import           System.OsString.Internal.Types (OsString (..),
                                                 PosixString (..))
#endif

#else
import qualified Data.ByteString.Short          as BS
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           System.FilePath
#endif

import           Control.Monad.Catch            (MonadThrow)
import           Data.ByteString.Short          (ShortByteString)

type OsPathCompat =
#ifdef OS_PATH
  OsPath
#else
  FilePath
#endif

toShortByteString :: OsPathCompat -> ShortByteString
#ifdef OS_PATH
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
toShortByteString = getWindowsString . getOsString
#else
toShortByteString = getPosixString . getOsString
#endif
#else
toShortByteString = BS.toShort . T.encodeUtf8 . T.pack
#endif

fromShortByteString :: ShortByteString -> OsPathCompat
#ifdef OS_PATH
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
fromShortByteString = OsString . WindowsString
#else
fromShortByteString = OsString . PosixString
#endif
#else
fromShortByteString = T.unpack . T.decodeUtf8 . BS.fromShort
#endif

toFilePath :: MonadThrow m => OsPathCompat -> m FilePath
#ifdef OS_PATH
toFilePath = decodeUtf
#else
toFilePath = pure
#endif

fromFilePath :: MonadThrow m => FilePath -> m OsPathCompat
#ifdef OS_PATH
fromFilePath = encodeUtf
#else
fromFilePath = pure
#endif
