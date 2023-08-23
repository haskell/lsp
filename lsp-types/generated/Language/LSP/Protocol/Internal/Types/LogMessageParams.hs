-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.LogMessageParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.MessageType
import qualified Language.LSP.Protocol.Types.Common

{-|
The log message parameters.
-}
data LogMessageParams = LogMessageParams 
  { {-|
  The message type. See `MessageType`
  -}
  _type_ :: Language.LSP.Protocol.Internal.Types.MessageType.MessageType
  , {-|
  The actual message.
  -}
  _message :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON LogMessageParams)

instance Aeson.ToJSON LogMessageParams where
  toJSON (LogMessageParams arg0 arg1) = Aeson.object $ concat $  [["type" Aeson..= arg0]
    ,["message" Aeson..= arg1]]

instance Aeson.FromJSON LogMessageParams where
  parseJSON = Aeson.withObject "LogMessageParams" $ \arg -> LogMessageParams <$> arg Aeson..: "type" <*> arg Aeson..: "message"
