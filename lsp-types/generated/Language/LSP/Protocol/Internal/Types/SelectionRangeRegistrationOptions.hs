-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SelectionRangeRegistrationOptions where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.DocumentSelector
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data SelectionRangeRegistrationOptions = SelectionRangeRegistrationOptions 
  { {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  , {-|
  A document selector to identify the scope of the registration. If set to null
  the document selector provided on the client side will be used.
  -}
  _documentSelector :: (Language.LSP.Protocol.Internal.Types.DocumentSelector.DocumentSelector Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|
  The id used to register the request. The id can be used to deregister
  the request again. See also Registration#id.
  -}
  _id :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON SelectionRangeRegistrationOptions where
  toJSON (SelectionRangeRegistrationOptions arg0 arg1 arg2) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,["documentSelector" Aeson..= arg1]
    ,"id" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON SelectionRangeRegistrationOptions where
  parseJSON = Aeson.withObject "SelectionRangeRegistrationOptions" $ \arg -> SelectionRangeRegistrationOptions <$> arg Aeson..:! "workDoneProgress" <*> arg Aeson..: "documentSelector" <*> arg Aeson..:! "id"
