{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.NotebookDocumentFilter where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentFilterNotebookType
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentFilterPattern
import qualified Language.LSP.Protocol.Internal.Types.NotebookDocumentFilterScheme
import qualified Language.LSP.Protocol.Types.Common

{-|
A notebook document filter denotes a notebook document by
different properties. The properties will be match
against the notebook's URI (same as with documents)

@since 3.17.0
-}
newtype NotebookDocumentFilter = NotebookDocumentFilter (Language.LSP.Protocol.Internal.Types.NotebookDocumentFilterNotebookType.NotebookDocumentFilterNotebookType Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.NotebookDocumentFilterScheme.NotebookDocumentFilterScheme Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.NotebookDocumentFilterPattern.NotebookDocumentFilterPattern))
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON NotebookDocumentFilter)
