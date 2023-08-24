{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceEditClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.FailureHandlingKind
import qualified Language.LSP.Protocol.Internal.Types.ResourceOperationKind
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data WorkspaceEditClientCapabilities = WorkspaceEditClientCapabilities 
  { {-|
  The client supports versioned document changes in `WorkspaceEdit`s
  -}
  _documentChanges :: (Maybe Bool)
  , {-|
  The resource operations the client supports. Clients should at least
  support 'create', 'rename' and 'delete' files and folders.

  @since 3.13.0
  -}
  _resourceOperations :: (Maybe [Language.LSP.Protocol.Internal.Types.ResourceOperationKind.ResourceOperationKind])
  , {-|
  The failure handling strategy of a client if applying the workspace edit
  fails.

  @since 3.13.0
  -}
  _failureHandling :: (Maybe Language.LSP.Protocol.Internal.Types.FailureHandlingKind.FailureHandlingKind)
  , {-|
  Whether the client normalizes line endings to the client specific
  setting.
  If set to `true` the client will normalize line ending characters
  in a workspace edit to the client-specified new line
  character.

  @since 3.16.0
  -}
  _normalizesLineEndings :: (Maybe Bool)
  , {-|
  Whether the client in general supports change annotations on text edits,
  create file, rename file and delete file changes.

  @since 3.16.0
  -}
  _changeAnnotationSupport :: (Maybe (Row.Rec ("groupsOnLabel" Row..== (Maybe Bool) Row..+ Row.Empty)))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON WorkspaceEditClientCapabilities)

instance Aeson.ToJSON WorkspaceEditClientCapabilities where
  toJSON (WorkspaceEditClientCapabilities arg0 arg1 arg2 arg3 arg4) = Aeson.object $ concat $  ["documentChanges" Language.LSP.Protocol.Types.Common..=? arg0
    ,"resourceOperations" Language.LSP.Protocol.Types.Common..=? arg1
    ,"failureHandling" Language.LSP.Protocol.Types.Common..=? arg2
    ,"normalizesLineEndings" Language.LSP.Protocol.Types.Common..=? arg3
    ,"changeAnnotationSupport" Language.LSP.Protocol.Types.Common..=? arg4]

instance Aeson.FromJSON WorkspaceEditClientCapabilities where
  parseJSON = Aeson.withObject "WorkspaceEditClientCapabilities" $ \arg -> WorkspaceEditClientCapabilities <$> arg Aeson..:! "documentChanges" <*> arg Aeson..:! "resourceOperations" <*> arg Aeson..:! "failureHandling" <*> arg Aeson..:! "normalizesLineEndings" <*> arg Aeson..:! "changeAnnotationSupport"
