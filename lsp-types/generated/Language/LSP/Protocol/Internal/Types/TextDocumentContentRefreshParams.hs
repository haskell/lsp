{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentContentRefreshParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
Parameters for the `workspace/textDocumentContent/refresh` request.

@since 3.18.0
-}
data TextDocumentContentRefreshParams = TextDocumentContentRefreshParams 
  { {-|
  The uri of the text document to refresh.
  -}
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON TextDocumentContentRefreshParams)

instance Aeson.ToJSON TextDocumentContentRefreshParams where
  toJSON (TextDocumentContentRefreshParams arg0) = Aeson.object $ concat $  [["uri" Aeson..= arg0]]

instance Aeson.FromJSON TextDocumentContentRefreshParams where
  parseJSON = Aeson.withObject "TextDocumentContentRefreshParams" $ \arg -> TextDocumentContentRefreshParams <$> arg Aeson..: "uri"
