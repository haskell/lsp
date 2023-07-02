-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InitializeParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.ClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Internal.Types.TraceValues
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceFolder
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|

-}
data InitializeParams = InitializeParams 
  { {-|
  An optional token that a server can use to report work done progress.
  -}
  _workDoneToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|
  The process Id of the parent process that started
  the server.

  Is `null` if the process has not been started by another process.
  If the parent process is not alive then the server should exit.
  -}
  _processId :: (Language.LSP.Protocol.Types.Common.Int32 Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|
  Information about the client

  @since 3.15.0
  -}
  _clientInfo :: (Maybe (Row.Rec ("name" Row..== Data.Text.Text Row..+ ("version" Row..== (Maybe Data.Text.Text) Row..+ Row.Empty))))
  , {-|
  The locale the client is currently showing the user interface
  in. This must not necessarily be the locale of the operating
  system.

  Uses IETF language tags as the value's syntax
  (See https://en.wikipedia.org/wiki/IETF_language_tag)

  @since 3.16.0
  -}
  _locale :: (Maybe Data.Text.Text)
  , {-|
  The rootPath of the workspace. Is null
  if no folder is open.

  @deprecated in favour of rootUri.
  -}
  _rootPath :: (Maybe (Data.Text.Text Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null))
  , {-|
  The rootUri of the workspace. Is null if no
  folder is open. If both `rootPath` and `rootUri` are set
  `rootUri` wins.

  @deprecated in favour of workspaceFolders.
  -}
  _rootUri :: (Language.LSP.Protocol.Types.Uri.Uri Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|
  The capabilities provided by the client (editor or tool)
  -}
  _capabilities :: Language.LSP.Protocol.Internal.Types.ClientCapabilities.ClientCapabilities
  , {-|
  User provided initialization options.
  -}
  _initializationOptions :: (Maybe Data.Aeson.Value)
  , {-|
  The initial trace setting. If omitted trace is disabled ('off').
  -}
  _trace :: (Maybe Language.LSP.Protocol.Internal.Types.TraceValues.TraceValues)
  , {-|
  The workspace folders configured in the client when the server starts.

  This property is only available if the client supports workspace folders.
  It can be `null` if the client supports workspace folders but none are
  configured.

  @since 3.6.0
  -}
  _workspaceFolders :: (Maybe ([Language.LSP.Protocol.Internal.Types.WorkspaceFolder.WorkspaceFolder] Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON InitializeParams where
  toJSON (InitializeParams arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9) = Aeson.object $ concat $  ["workDoneToken" Language.LSP.Protocol.Types.Common..=? arg0
    ,["processId" Aeson..= arg1]
    ,"clientInfo" Language.LSP.Protocol.Types.Common..=? arg2
    ,"locale" Language.LSP.Protocol.Types.Common..=? arg3
    ,"rootPath" Language.LSP.Protocol.Types.Common..=? arg4
    ,["rootUri" Aeson..= arg5]
    ,["capabilities" Aeson..= arg6]
    ,"initializationOptions" Language.LSP.Protocol.Types.Common..=? arg7
    ,"trace" Language.LSP.Protocol.Types.Common..=? arg8
    ,"workspaceFolders" Language.LSP.Protocol.Types.Common..=? arg9]

instance Aeson.FromJSON InitializeParams where
  parseJSON = Aeson.withObject "InitializeParams" $ \arg -> InitializeParams <$> arg Aeson..:! "workDoneToken" <*> arg Aeson..: "processId" <*> arg Aeson..:! "clientInfo" <*> arg Aeson..:! "locale" <*> arg Aeson..:! "rootPath" <*> arg Aeson..: "rootUri" <*> arg Aeson..: "capabilities" <*> arg Aeson..:! "initializationOptions" <*> arg Aeson..:! "trace" <*> arg Aeson..:! "workspaceFolders"
