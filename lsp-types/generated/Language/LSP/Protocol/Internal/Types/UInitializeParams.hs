{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.UInitializeParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.ClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.ClientInfo
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Internal.Types.TraceValue
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
The initialize parameters
-}
data UInitializeParams = UInitializeParams 
  { {-|
  An optional token that a server can use to report work done progress.
  -}
  workDoneToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|
  The process Id of the parent process that started
  the server.

  Is `null` if the process has not been started by another process.
  If the parent process is not alive then the server should exit.
  -}
  processId :: (Language.LSP.Protocol.Types.Common.Int32 Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|
  Information about the client

  @since 3.15.0
  -}
  clientInfo :: (Maybe Language.LSP.Protocol.Internal.Types.ClientInfo.ClientInfo)
  , {-|
  The locale the client is currently showing the user interface
  in. This must not necessarily be the locale of the operating
  system.

  Uses IETF language tags as the value's syntax
  (See https://en.wikipedia.org/wiki/IETF_language_tag)

  @since 3.16.0
  -}
  locale :: (Maybe Data.Text.Text)
  , {-|
  The rootPath of the workspace. Is null
  if no folder is open.

  @deprecated in favour of rootUri.
  -}
  rootPath :: (Maybe (Data.Text.Text Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null))
  , {-|
  The rootUri of the workspace. Is null if no
  folder is open. If both `rootPath` and `rootUri` are set
  `rootUri` wins.

  @deprecated in favour of workspaceFolders.
  -}
  rootUri :: (Language.LSP.Protocol.Types.Uri.Uri Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|
  The capabilities provided by the client (editor or tool)
  -}
  capabilities :: Language.LSP.Protocol.Internal.Types.ClientCapabilities.ClientCapabilities
  , {-|
  User provided initialization options.
  -}
  initializationOptions :: (Maybe Data.Aeson.Value)
  , {-|
  The initial trace setting. If omitted trace is disabled ('off').
  -}
  trace :: (Maybe Language.LSP.Protocol.Internal.Types.TraceValue.TraceValue)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON UInitializeParams)

instance Aeson.ToJSON UInitializeParams where
  toJSON (UInitializeParams arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) = Aeson.object $ concat $  ["workDoneToken" Language.LSP.Protocol.Types.Common..=? arg0
    ,["processId" Aeson..= arg1]
    ,"clientInfo" Language.LSP.Protocol.Types.Common..=? arg2
    ,"locale" Language.LSP.Protocol.Types.Common..=? arg3
    ,"rootPath" Language.LSP.Protocol.Types.Common..=? arg4
    ,["rootUri" Aeson..= arg5]
    ,["capabilities" Aeson..= arg6]
    ,"initializationOptions" Language.LSP.Protocol.Types.Common..=? arg7
    ,"trace" Language.LSP.Protocol.Types.Common..=? arg8]

instance Aeson.FromJSON UInitializeParams where
  parseJSON = Aeson.withObject "_InitializeParams" $ \arg -> UInitializeParams <$> arg Language.LSP.Protocol.Types.Common..:!? "workDoneToken" <*> arg Aeson..: "processId" <*> arg Language.LSP.Protocol.Types.Common..:!? "clientInfo" <*> arg Language.LSP.Protocol.Types.Common..:!? "locale" <*> arg Language.LSP.Protocol.Types.Common..:!? "rootPath" <*> arg Aeson..: "rootUri" <*> arg Aeson..: "capabilities" <*> arg Language.LSP.Protocol.Types.Common..:!? "initializationOptions" <*> arg Language.LSP.Protocol.Types.Common..:!? "trace"
