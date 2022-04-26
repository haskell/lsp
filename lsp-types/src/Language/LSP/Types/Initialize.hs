{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.LSP.Types.Initialize where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Types.ClientCapabilities
import Language.LSP.Types.Common
import Language.LSP.Types.Progress
import Language.LSP.Types.ServerCapabilities
import Language.LSP.Types.Uri
import Language.LSP.Types.Utils
import Language.LSP.Types.WorkspaceFolders

data Trace = TraceOff | TraceMessages | TraceVerbose
           deriving (Show, Read, Eq)

instance ToJSON Trace where
  toJSON TraceOff      = String (T.pack "off")
  toJSON TraceMessages = String (T.pack "messages")
  toJSON TraceVerbose  = String (T.pack "verbose")

instance FromJSON Trace where
  parseJSON (String s) = case T.unpack s of
    "off"      -> return TraceOff
    "messages" -> return TraceMessages
    "verbose"  -> return TraceVerbose
    _          -> fail "Trace"
  parseJSON _          = fail "Trace"

data ClientInfo =
  ClientInfo
  { -- | The name of the client as defined by the client.
    _name    :: Text
    -- | The client's version as defined by the client.
  , _version :: Maybe Text
  } deriving (Show, Read, Eq)
deriveJSON lspOptions ''ClientInfo

makeExtendingDatatype "InitializeParams" [''WorkDoneProgressParams]
  [ ("_processId",             [t| MaybeN Int32|])
  , ("_clientInfo",            [t| Maybe ClientInfo |])
  , ("_rootPath",              [t| MaybeN Text |])
  , ("_rootUri",               [t| MaybeN Uri |])
  , ("_initializationOptions", [t| Maybe Value |])
  , ("_capabilities",          [t| ClientCapabilities |])
  , ("_trace",                 [t| Maybe Trace |])
  , ("_workspaceFolders",      [t| MaybeN (List WorkspaceFolder) |])
  ]

deriveJSON lspOptions ''InitializeParams

data InitializeError =
  InitializeError
    { _retry :: Bool
    } deriving (Read, Show, Eq)

deriveJSON lspOptions ''InitializeError

data ServerInfo =
  ServerInfo
  { -- | The name of the server as defined by the server.
    _name    :: Text
    -- | The server's version as defined by the server.
  , _version :: Maybe Text
  } deriving (Show, Read, Eq)
deriveJSON lspOptions ''ServerInfo

data InitializeResult =
  InitializeResult
  { -- | The capabilities the language server provides.
    _capabilities :: ServerCapabilities
    -- | Information about the server.
    -- Since LSP 3.15.0
  , _serverInfo   :: Maybe ServerInfo
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''InitializeResult

-- ---------------------------------------------------------------------

data InitializedParams =
  InitializedParams
  {
  } deriving (Show, Read, Eq)

instance FromJSON InitializedParams where
  parseJSON (Object _) = pure InitializedParams
  parseJSON _          = fail "InitializedParams"

instance ToJSON InitializedParams where
  toJSON InitializedParams = Object mempty
