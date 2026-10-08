{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlineCompletionContext where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.InlineCompletionTriggerKind
import qualified Language.LSP.Protocol.Internal.Types.SelectedCompletionInfo
import qualified Language.LSP.Protocol.Types.Common

{-|
Provides information about the context in which an inline completion was requested.

@since 3.18.0
-}
data InlineCompletionContext = InlineCompletionContext 
  { {-|
  Describes how the inline completion was triggered.
  -}
  _triggerKind :: Language.LSP.Protocol.Internal.Types.InlineCompletionTriggerKind.InlineCompletionTriggerKind
  , {-|
  Provides information about the currently selected item in the autocomplete widget if it is visible.
  -}
  _selectedCompletionInfo :: (Maybe Language.LSP.Protocol.Internal.Types.SelectedCompletionInfo.SelectedCompletionInfo)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON InlineCompletionContext)

instance Aeson.ToJSON InlineCompletionContext where
  toJSON (InlineCompletionContext arg0 arg1) = Aeson.object $ concat $  [["triggerKind" Aeson..= arg0]
    ,"selectedCompletionInfo" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON InlineCompletionContext where
  parseJSON = Aeson.withObject "InlineCompletionContext" $ \arg -> InlineCompletionContext <$> arg Aeson..: "triggerKind" <*> arg Language.LSP.Protocol.Types.Common..:!? "selectedCompletionInfo"
