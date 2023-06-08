-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.PreviousResultId where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
A previous result id in a workspace pull request.

@since 3.17.0
-}
data PreviousResultId = PreviousResultId 
  { {-|
  The URI for which the client knowns a
  result id.
  -}
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  The value of the previous result id.
  -}
  _value :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON PreviousResultId where
  toJSON (PreviousResultId arg0 arg1) = Aeson.object $ concat $  [["uri" Aeson..= arg0]
    ,["value" Aeson..= arg1]]

instance Aeson.FromJSON PreviousResultId where
  parseJSON = Aeson.withObject "PreviousResultId" $ \arg -> PreviousResultId <$> arg Aeson..: "uri" <*> arg Aeson..: "value"