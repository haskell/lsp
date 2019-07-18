{-# LANGUAGE CPP #-}
-- For some reason ghc warns about not using
-- Control.Monad.IO.Class but it's needed for
-- MonadIO
{-# OPTIONS_GHC -Wunused-imports #-}
module Language.Haskell.LSP.Test.Compat where

import Data.Maybe
import System.IO

#if MIN_VERSION_process(1,6,3)
import System.Process hiding (getPid, cleanupProcess)
import qualified System.Process (getPid, cleanupProcess)
#else
import System.Process
import System.Process.Internals
import Control.Concurrent.MVar
#endif

#ifdef mingw32_HOST_OS
import qualified System.Win32.Process
#else
import qualified System.Posix.Process
#endif


getCurrentProcessID :: IO Int
#ifdef mingw32_HOST_OS
getCurrentProcessID = fromIntegral <$> System.Win32.Process.getCurrentProcessId
#else
getCurrentProcessID = fromIntegral <$> System.Posix.Process.getProcessID
#endif

getProcessID :: ProcessHandle -> IO Int
getProcessID p = fromIntegral . fromJust <$> getProcessID' p
  where
#if MIN_VERSION_process(1,6,3)
  getProcessID' = System.Process.getPid
#else
#if MIN_VERSION_process(1,6,0)
  getProcessID' (ProcessHandle mh _ _) = do
#else
  getProcessID' (ProcessHandle mh _) = do
#endif
    p_ <- readMVar mh
    case p_ of
#ifdef mingw32_HOST_OS
      OpenHandle h -> do
        pid <- System.Win32.Process.getProcessId h
        return $ Just pid
#else
      OpenHandle pid -> return $ Just pid
#endif
      _ -> return Nothing
#endif

cleanupRunningProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
cleanupRunningProcess p@(_, _, _, ph) =
  getProcessExitCode ph >>= maybe (cleanupProcess p) (const $ return ())

cleanupProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
#if MIN_VERSION_process(1,6,3)
cleanupProcess = System.Process.cleanupProcess
#else
cleanupProcess (mb_stdin, mb_stdout, mb_stderr, ph) = do

    terminateProcess ph
    -- Note, it's important that other threads that might be reading/writing
    -- these handles also get killed off, since otherwise they might be holding
    -- the handle lock and prevent us from closing, leading to deadlock.
    maybe (return ()) hClose mb_stdin
    maybe (return ()) hClose mb_stdout
    maybe (return ()) hClose mb_stderr

    return ()
#endif
