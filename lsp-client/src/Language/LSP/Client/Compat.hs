{-# LANGUAGE CPP #-}

module Language.LSP.Client.Compat where

import Language.LSP.Types
import Prelude

#ifdef mingw32_HOST_OS
import System.Win32.Process qualified
#else
import System.Posix.Process qualified
#endif

getCurrentProcessID :: IO Int
#ifdef mingw32_HOST_OS
getCurrentProcessID = fromIntegral <$> System.Win32.Process.getCurrentProcessID
#else
getCurrentProcessID = fromIntegral <$> System.Posix.Process.getProcessID
#endif

lspClientInfo :: ClientInfo
lspClientInfo = ClientInfo "lsp-client" (Just CURRENT_PACKAGE_VERSION)
