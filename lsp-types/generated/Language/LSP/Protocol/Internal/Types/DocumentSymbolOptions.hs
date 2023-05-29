-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentSymbolOptions where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
Provider options for a `DocumentSymbolRequest`.
-}
data DocumentSymbolOptions = DocumentSymbolOptions 
  { {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  , {-|
  A human-readable string that is shown when multiple outlines trees
  are shown for the same document.

  @since 3.16.0
  -}
  _label :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON DocumentSymbolOptions where
  toJSON (DocumentSymbolOptions arg0 arg1) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,"label" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON DocumentSymbolOptions where
  parseJSON = Aeson.withObject "DocumentSymbolOptions" $ \arg -> DocumentSymbolOptions <$> arg Aeson..:! "workDoneProgress" <*> arg Aeson..:! "label"