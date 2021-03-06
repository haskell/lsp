{-# LANGUAGE TypeOperators #-}
module Language.LSP.Server
  ( module Language.LSP.Server.Control
  , VFSData(..)
  , ServerDefinition(..)

  -- * Handlers
  , Handlers(..)
  , Handler
  , transmuteHandlers
  , mapHandlers
  , notificationHandler
  , requestHandler
  , ClientMessageHandler(..)

  , Options(..)
  , defaultOptions

  -- * LspT and LspM
  , LspT(..)
  , LspM
  , MonadLsp(..)
  , runLspT
  , LanguageContextEnv(..)
  , type (<~>)(..)

  , getClientCapabilities
  , getConfig
  , getRootPath
  , getWorkspaceFolders

  , sendRequest
  , sendNotification

  -- * VFS
  , getVirtualFile
  , getVirtualFiles
  , persistVirtualFile
  , getVersionedTextDoc
  , reverseFileMap

  -- * Diagnostics
  , publishDiagnostics
  , flushDiagnosticsBySource

  -- * Progress
  , withProgress
  , withIndefiniteProgress
  , ProgressAmount(..)
  , ProgressCancellable(..)
  , ProgressCancelledException

  -- * Dynamic registration
  , registerCapability
  , unregisterCapability
  , RegistrationToken

  , setupLogger
  , reverseSortEdit
  ) where

import Language.LSP.Server.Control
import Language.LSP.Server.Core
