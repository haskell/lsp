-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentRangeFormattingOptions where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
Provider options for a `DocumentRangeFormattingRequest`.
-}
data DocumentRangeFormattingOptions = DocumentRangeFormattingOptions 
  { {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON DocumentRangeFormattingOptions where
  toJSON (DocumentRangeFormattingOptions arg0) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON DocumentRangeFormattingOptions where
  parseJSON = Aeson.withObject "DocumentRangeFormattingOptions" $ \arg -> DocumentRangeFormattingOptions <$> arg Aeson..:! "workDoneProgress"
