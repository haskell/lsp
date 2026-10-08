{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentContentRegistrationOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
Text document content provider registration options.

@since 3.18.0
-}
data TextDocumentContentRegistrationOptions = TextDocumentContentRegistrationOptions 
  { {-|
  The schemes for which the server provides content.
  -}
  _schemes :: [Data.Text.Text]
  , {-|
  The id used to register the request. The id can be used to deregister
  the request again. See also Registration#id.
  -}
  _id :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON TextDocumentContentRegistrationOptions)

instance Aeson.ToJSON TextDocumentContentRegistrationOptions where
  toJSON (TextDocumentContentRegistrationOptions arg0 arg1) = Aeson.object $ concat $  [["schemes" Aeson..= arg0]
    ,"id" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON TextDocumentContentRegistrationOptions where
  parseJSON = Aeson.withObject "TextDocumentContentRegistrationOptions" $ \arg -> TextDocumentContentRegistrationOptions <$> arg Aeson..: "schemes" <*> arg Language.LSP.Protocol.Types.Common..:!? "id"
