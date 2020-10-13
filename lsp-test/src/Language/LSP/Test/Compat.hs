{-# LANGUAGE CPP, OverloadedStrings #-}
-- For some reason ghc warns about not using
-- Control.Monad.IO.Class but it's needed for
-- MonadIO
{-# OPTIONS_GHC -Wunused-imports #-}
module Language.LSP.Test.Compat where

import Data.Maybe
import System.IO
import Language.LSP.Types

#if MIN_VERSION_process(1,6,3)
-- We have to hide cleanupProcess for process-1.6.3.0
-- cause it is in the public api for 1.6.3.0 versions
-- shipped with ghc >= 8.6 and < 8.6.4
import System.Process hiding (getPid, cleanupProcess, withCreateProcess)
# if MIN_VERSION_process(1,6,4)
import qualified System.Process (getPid, cleanupProcess, withCreateProcess)
# else
import Foreign.C.Error
import GHC.IO.Exception ( IOErrorType(..), IOException(..) )

import qualified System.Process (getPid)
import qualified Control.Exception as C
# endif
#else
import Control.Concurrent.MVar
import Foreign.C.Error
import GHC.IO.Exception ( IOErrorType(..), IOException(..) )
import System.Process hiding (withCreateProcess)
import System.Process.Internals

import qualified Control.Exception as C
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

cleanupProcess
  :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()

withCreateProcess
  :: CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a

#if MIN_VERSION_process(1,6,4)

cleanupProcess = System.Process.cleanupProcess

withCreateProcess = System.Process.withCreateProcess

#else

cleanupProcess (mb_stdin, mb_stdout, mb_stderr, ph) = do
    -- We ignore the spurious "permission denied" error in windows:
    --   see https://github.com/haskell/process/issues/110
    ignorePermDenied $ terminateProcess ph
    -- Note, it's important that other threads that might be reading/writing
    -- these handles also get killed off, since otherwise they might be holding
    -- the handle lock and prevent us from closing, leading to deadlock.
    maybe (return ()) (ignoreSigPipe . hClose) mb_stdin
    maybe (return ()) hClose mb_stdout
    maybe (return ()) hClose mb_stderr

    return ()
  where ignoreSigPipe = ignoreIOError ResourceVanished ePIPE
        ignorePermDenied = ignoreIOError PermissionDenied eACCES
    
ignoreIOError :: IOErrorType -> Errno -> IO () -> IO ()
ignoreIOError ioErrorType errno =
  C.handle $ \e -> case e of
                     IOError { ioe_type  = iot
                             , ioe_errno = Just ioe }
                       | iot == ioErrorType && Errno ioe == errno -> return ()
                     _ -> C.throwIO e

withCreateProcess c action =
  C.bracket (createProcess c) cleanupProcess
            (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)

#endif


lspTestClientInfo :: ClientInfo
lspTestClientInfo = ClientInfo "lsp-test" (Just CURRENT_PACKAGE_VERSION)
