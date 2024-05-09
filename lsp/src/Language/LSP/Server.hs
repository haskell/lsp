{-# LANGUAGE ExplicitNamespaces #-}

module Language.LSP.Server (
  module Language.LSP.Server.Control,
  VFSData (..),
  ServerDefinition (..),

  -- * Handlers
  Handlers (..),
  Handler,
  transmuteHandlers,
  mapHandlers,
  notificationHandler,
  requestHandler,
  ClientMessageHandler (..),
  Options (..),
  defaultOptions,

  -- * LspT and LspM
  LspT (..),
  LspM,
  MonadLsp (..),
  runLspT,
  LanguageContextEnv (..),
  type (<~>) (..),
  getClientCapabilities,
  getConfig,
  setConfig,
  getRootPath,
  getWorkspaceFolders,
  sendRequest,
  sendNotification,

  -- * Config
  requestConfigUpdate,
  tryChangeConfig,

  -- * Shutdown
  isShuttingDown,
  waitShuttingDown,

  -- * VFS
  getVirtualFile,
  getVirtualFiles,
  persistVirtualFile,
  getVersionedTextDoc,
  reverseFileMap,
  snapshotVirtualFiles,

  -- * Diagnostics
  publishDiagnostics,
  flushDiagnosticsBySource,

  -- * Progress
  withProgress,
  withIndefiniteProgress,
  ProgressAmount (..),
  ProgressCancellable (..),
  ProgressCancelledException,

  -- * Dynamic registration
  registerCapability,
  unregisterCapability,
  RegistrationToken,
  reverseSortEdit,
) where

import Language.LSP.Server.Control
import Language.LSP.Server.Core
import Language.LSP.Server.Progress
