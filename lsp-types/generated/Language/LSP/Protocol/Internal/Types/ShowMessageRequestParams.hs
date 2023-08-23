-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ShowMessageRequestParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.MessageActionItem
import qualified Language.LSP.Protocol.Internal.Types.MessageType
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data ShowMessageRequestParams = ShowMessageRequestParams 
  { {-|
  The message type. See `MessageType`
  -}
  _type_ :: Language.LSP.Protocol.Internal.Types.MessageType.MessageType
  , {-|
  The actual message.
  -}
  _message :: Data.Text.Text
  , {-|
  The message action items to present.
  -}
  _actions :: (Maybe [Language.LSP.Protocol.Internal.Types.MessageActionItem.MessageActionItem])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ShowMessageRequestParams)

instance Aeson.ToJSON ShowMessageRequestParams where
  toJSON (ShowMessageRequestParams arg0 arg1 arg2) = Aeson.object $ concat $  [["type" Aeson..= arg0]
    ,["message" Aeson..= arg1]
    ,"actions" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON ShowMessageRequestParams where
  parseJSON = Aeson.withObject "ShowMessageRequestParams" $ \arg -> ShowMessageRequestParams <$> arg Aeson..: "type" <*> arg Aeson..: "message" <*> arg Aeson..:! "actions"
