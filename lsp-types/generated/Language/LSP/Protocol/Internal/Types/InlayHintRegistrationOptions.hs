-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlayHintRegistrationOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.DocumentSelector
import qualified Language.LSP.Protocol.Types.Common

{-|
Inlay hint options used during static or dynamic registration.

@since 3.17.0
-}
data InlayHintRegistrationOptions = InlayHintRegistrationOptions 
  { {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  , {-|
  The server provides support to resolve additional
  information for an inlay hint item.
  -}
  _resolveProvider :: (Maybe Bool)
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
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON InlayHintRegistrationOptions where
  toJSON (InlayHintRegistrationOptions arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,"resolveProvider" Language.LSP.Protocol.Types.Common..=? arg1
    ,["documentSelector" Aeson..= arg2]
    ,"id" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON InlayHintRegistrationOptions where
  parseJSON = Aeson.withObject "InlayHintRegistrationOptions" $ \arg -> InlayHintRegistrationOptions <$> arg Aeson..:! "workDoneProgress" <*> arg Aeson..:! "resolveProvider" <*> arg Aeson..: "documentSelector" <*> arg Aeson..:! "id"
