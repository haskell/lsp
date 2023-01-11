-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.LinkedEditingRangeRegistrationOptions where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.DocumentSelector
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data LinkedEditingRangeRegistrationOptions = LinkedEditingRangeRegistrationOptions 
  { {-|
  A document selector to identify the scope of the registration. If set to null
  the document selector provided on the client side will be used.

  -}
  _documentSelector :: (Language.LSP.Protocol.Internal.Types.DocumentSelector.DocumentSelector Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  , {-|
  The id used to register the request. The id can be used to deregister
  the request again. See also Registration#id.

  -}
  _id :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON LinkedEditingRangeRegistrationOptions where
  toJSON (LinkedEditingRangeRegistrationOptions arg0 arg1 arg2) = Aeson.object $ concat $  [["documentSelector" Aeson..= arg0]
    ,"workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg1
    ,"id" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON LinkedEditingRangeRegistrationOptions where
  parseJSON = Aeson.withObject "LinkedEditingRangeRegistrationOptions" $ \arg -> LinkedEditingRangeRegistrationOptions <$> arg Aeson..: "documentSelector" <*> arg Aeson..:! "workDoneProgress" <*> arg Aeson..:! "id"