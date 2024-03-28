{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientCompletionItemOptionsKind where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.CompletionItemKind
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.18.0
@proposed
-}
data ClientCompletionItemOptionsKind = ClientCompletionItemOptionsKind 
  { {-|
  The completion item kind values the client supports. When this
  property exists the client also guarantees that it will
  handle values outside its set gracefully and falls back
  to a default value when unknown.

  If this property is not present the client only supports
  the completion items kinds from `Text` to `Reference` as defined in
  the initial version of the protocol.
  -}
  valueSet :: (Maybe [Language.LSP.Protocol.Internal.Types.CompletionItemKind.CompletionItemKind])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientCompletionItemOptionsKind)

instance Aeson.ToJSON ClientCompletionItemOptionsKind where
  toJSON (ClientCompletionItemOptionsKind arg0) = Aeson.object $ concat $  ["valueSet" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON ClientCompletionItemOptionsKind where
  parseJSON = Aeson.withObject "ClientCompletionItemOptionsKind" $ \arg -> ClientCompletionItemOptionsKind <$> arg Language.LSP.Protocol.Types.Common..:!? "valueSet"
