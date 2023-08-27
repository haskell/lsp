{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookDocumentClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncClientCapabilities
import qualified Language.LSP.Protocol.Types.Common

{-|
Capabilities specific to the notebook document support.

@since 3.17.0
-}
data NotebookDocumentClientCapabilities = NotebookDocumentClientCapabilities 
  { {-|
  Capabilities specific to notebook document synchronization

  @since 3.17.0
  -}
  _synchronization :: Language.LSP.Protocol.Internal.Types.NotebookDocumentSyncClientCapabilities.NotebookDocumentSyncClientCapabilities
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON NotebookDocumentClientCapabilities)

instance Aeson.ToJSON NotebookDocumentClientCapabilities where
  toJSON (NotebookDocumentClientCapabilities arg0) = Aeson.object $ concat $  [["synchronization" Aeson..= arg0]]

instance Aeson.FromJSON NotebookDocumentClientCapabilities where
  parseJSON = Aeson.withObject "NotebookDocumentClientCapabilities" $ \arg -> NotebookDocumentClientCapabilities <$> arg Aeson..: "synchronization"
