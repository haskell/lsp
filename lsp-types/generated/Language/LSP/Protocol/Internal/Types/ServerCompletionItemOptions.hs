{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ServerCompletionItemOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.18.0
@proposed
-}
data ServerCompletionItemOptions = ServerCompletionItemOptions 
  { {-|
  The server has support for completion item label
  details (see also `CompletionItemLabelDetails`) when
  receiving a completion item in a resolve call.

  @since 3.17.0
  -}
  labelDetailsSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ServerCompletionItemOptions)

instance Aeson.ToJSON ServerCompletionItemOptions where
  toJSON (ServerCompletionItemOptions arg0) = Aeson.object $ concat $  ["labelDetailsSupport" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON ServerCompletionItemOptions where
  parseJSON = Aeson.withObject "ServerCompletionItemOptions" $ \arg -> ServerCompletionItemOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "labelDetailsSupport"
