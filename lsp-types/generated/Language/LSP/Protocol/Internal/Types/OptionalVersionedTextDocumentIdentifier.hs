-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.OptionalVersionedTextDocumentIdentifier where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
A text document identifier to optionally denote a specific version of a text document.
-}
data OptionalVersionedTextDocumentIdentifier = OptionalVersionedTextDocumentIdentifier 
  { {-|
  The text document's uri.
  -}
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  The version number of this document. If a versioned text document identifier
  is sent from the server to the client and the file is not open in the editor
  (the server has not received an open notification before) the server can send
  `null` to indicate that the version is unknown and the content on disk is the
  truth (as specified with document content ownership).
  -}
  _version :: (Language.LSP.Protocol.Types.Common.Int32 Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON OptionalVersionedTextDocumentIdentifier where
  toJSON (OptionalVersionedTextDocumentIdentifier arg0 arg1) = Aeson.object $ concat $  [["uri" Aeson..= arg0]
    ,["version" Aeson..= arg1]]

instance Aeson.FromJSON OptionalVersionedTextDocumentIdentifier where
  parseJSON = Aeson.withObject "OptionalVersionedTextDocumentIdentifier" $ \arg -> OptionalVersionedTextDocumentIdentifier <$> arg Aeson..: "uri" <*> arg Aeson..: "version"