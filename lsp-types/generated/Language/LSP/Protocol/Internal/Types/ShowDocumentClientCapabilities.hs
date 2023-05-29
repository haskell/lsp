-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ShowDocumentClientCapabilities where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
Client capabilities for the showDocument request.

@since 3.16.0
-}
data ShowDocumentClientCapabilities = ShowDocumentClientCapabilities 
  { {-|
  The client has support for the showDocument
  request.
  -}
  _support :: Bool
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON ShowDocumentClientCapabilities where
  toJSON (ShowDocumentClientCapabilities arg0) = Aeson.object $ concat $  [["support" Aeson..= arg0]]

instance Aeson.FromJSON ShowDocumentClientCapabilities where
  parseJSON = Aeson.withObject "ShowDocumentClientCapabilities" $ \arg -> ShowDocumentClientCapabilities <$> arg Aeson..: "support"