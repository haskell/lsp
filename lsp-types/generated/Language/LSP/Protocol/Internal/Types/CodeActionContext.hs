{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CodeActionContext where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.CodeActionKind
import qualified Language.LSP.Protocol.Internal.Types.CodeActionTriggerKind
import qualified Language.LSP.Protocol.Internal.Types.Diagnostic
import qualified Language.LSP.Protocol.Types.Common

{-|
Contains additional diagnostic information about the context in which
a `CodeActionProvider.provideCodeActions` is run.
-}
data CodeActionContext = CodeActionContext 
  { {-|
  An array of diagnostics known on the client side overlapping the range provided to the
  `textDocument/codeAction` request. They are provided so that the server knows which
  errors are currently presented to the user for the given range. There is no guarantee
  that these accurately reflect the error state of the resource. The primary parameter
  to compute code actions is the provided range.
  -}
  diagnostics :: [Language.LSP.Protocol.Internal.Types.Diagnostic.Diagnostic]
  , {-|
  Requested kind of actions to return.

  Actions not of this kind are filtered out by the client before being shown. So servers
  can omit computing them.
  -}
  only :: (Maybe [Language.LSP.Protocol.Internal.Types.CodeActionKind.CodeActionKind])
  , {-|
  The reason why code actions were requested.

  @since 3.17.0
  -}
  triggerKind :: (Maybe Language.LSP.Protocol.Internal.Types.CodeActionTriggerKind.CodeActionTriggerKind)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CodeActionContext)

instance Aeson.ToJSON CodeActionContext where
  toJSON (CodeActionContext arg0 arg1 arg2) = Aeson.object $ concat $  [["diagnostics" Aeson..= arg0]
    ,"only" Language.LSP.Protocol.Types.Common..=? arg1
    ,"triggerKind" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON CodeActionContext where
  parseJSON = Aeson.withObject "CodeActionContext" $ \arg -> CodeActionContext <$> arg Aeson..: "diagnostics" <*> arg Language.LSP.Protocol.Types.Common..:!? "only" <*> arg Language.LSP.Protocol.Types.Common..:!? "triggerKind"
