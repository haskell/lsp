{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentFormattingOptions where

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
Provider options for a `DocumentFormattingRequest`.
-}
data DocumentFormattingOptions = DocumentFormattingOptions 
  { {-|

  -}
  workDoneProgress :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DocumentFormattingOptions)

instance Aeson.ToJSON DocumentFormattingOptions where
  toJSON (DocumentFormattingOptions arg0) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON DocumentFormattingOptions where
  parseJSON = Aeson.withObject "DocumentFormattingOptions" $ \arg -> DocumentFormattingOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "workDoneProgress"
