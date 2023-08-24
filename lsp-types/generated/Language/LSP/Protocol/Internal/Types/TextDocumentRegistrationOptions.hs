-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentRegistrationOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.DocumentSelector
import qualified Language.LSP.Protocol.Types.Common

{-|
General text document registration options.
-}
data TextDocumentRegistrationOptions = TextDocumentRegistrationOptions 
  { {-|
  A document selector to identify the scope of the registration. If set to null
  the document selector provided on the client side will be used.
  -}
  _documentSelector :: (Language.LSP.Protocol.Internal.Types.DocumentSelector.DocumentSelector Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON TextDocumentRegistrationOptions)

instance Aeson.ToJSON TextDocumentRegistrationOptions where
  toJSON (TextDocumentRegistrationOptions arg0) = Aeson.object $ concat $  [["documentSelector" Aeson..= arg0]]

instance Aeson.FromJSON TextDocumentRegistrationOptions where
  parseJSON = Aeson.withObject "TextDocumentRegistrationOptions" $ \arg -> TextDocumentRegistrationOptions <$> arg Aeson..: "documentSelector"
