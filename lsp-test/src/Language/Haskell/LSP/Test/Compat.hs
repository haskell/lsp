{-# LANGUAGE CPP #-}
module Language.Haskell.LSP.Test.Compat where

import Control.Concurrent.Chan
import Control.Monad.IO.Class
import Data.Conduit

#ifdef mingw32_HOST_OS

import qualified System.Win32.Process as P (getCurrentProcessId)
getProcessID :: IO Int
getProcessID = fromIntegral <$> P.getCurrentProcessId

#else

import qualified System.Posix.Process as P (getProcessID)
getProcessID :: IO Int
getProcessID = fromIntegral <$> P.getProcessID

#endif

#if MIN_VERSION_conduit(1,3,0)
chanSource :: MonadIO m => Chan o -> ConduitT i o m b
#else
chanSource :: MonadIO m => Chan o -> ConduitM i o m b
#endif
chanSource c = do
  x <- liftIO $ readChan c
  yield x
  chanSource c
