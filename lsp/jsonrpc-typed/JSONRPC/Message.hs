{-# LANGUAGE TemplateHaskell #-}

module JSONRPC.Message where

import Language.LSP.Protocol.Utils.Misc (ViaJSON (..))

import Data.Aeson hiding (Null)
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics
import JSONRPC.Id
import JSONRPC.Method
import JSONRPC.Utils
import Language.LSP.Protocol.Types (Int32, Null, type (|?))
import Language.LSP.Protocol.Types qualified as LSP
import Prettyprinter
import Prelude hiding (id, error)
import Control.Monad

-- | Notification message type as defined in the spec.
data NotificationMessage = NotificationMessage
  { method :: Method
  , params :: Maybe Value
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON NotificationMessage where
  toJSON NotificationMessage{method,params} =
    object $
      ["jsonrpc" .= ("2.0" :: Text)
      , "method" .= method
      -- TODO: Can't use .?= yet, must be a nicer way!
      ] ++ case params of { Nothing -> []; Just p -> ["params" .= p] }

instance FromJSON NotificationMessage where
  parseJSON = withObject "NotificationMessage" $ \o -> do
    (jsonrpc :: Text) <- o .: "jsonrpc"
    method <- o .: "method"
    params <- o LSP..:!? "params"
    unless (jsonrpc == "2.0") $ fail "The LSP library only supports JSONRPC 2.0"
    pure $ NotificationMessage method params

deriving via ViaJSON NotificationMessage instance Pretty NotificationMessage

-- | Request message type as defined in the spec.
data RequestMessage = RequestMessage
  { id :: Id
  , method :: Method
  , params :: Maybe Value
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON RequestMessage where
  toJSON RequestMessage{id, method,params} =
    object $
      ["jsonrpc" .= ("2.0" :: Text)
      , "id" .= id
      , "method" .= method
      ] ++ case params of { Nothing -> []; Just p -> ["params" .= p] }

instance FromJSON RequestMessage where
  parseJSON = withObject "RequestMessage" $ \o -> do
    (jsonrpc :: Text) <- o .: "jsonrpc"
    id <- o .: "id"
    method <- o .: "method"
    params <- o LSP..:!? "params"
    unless (jsonrpc == "2.0") $ fail "The LSP library only supports JSONRPC 2.0"
    pure $ RequestMessage id method params

deriving via ViaJSON RequestMessage instance Pretty RequestMessage

-- | Response error type as defined in the spec.
data ResponseError = ResponseError
  { code :: Int32
  , message :: Text
  , data_ :: Maybe Value
  }
  deriving stock (Show, Eq, Ord, Generic)

deriveToJSON jsonOptions ''ResponseError

instance FromJSON ResponseError where
  parseJSON = withObject "ResponseError" $ \v ->
    ResponseError
      <$> v .: "code"
      <*> v .: "message"
      -- TODO: others should be like this
      <*> v LSP..:!? "data"

deriving via ViaJSON ResponseError instance Pretty ResponseError

-- | Response message type as defined in the spec.
data ResponseMessage = ResponseMessage
  { id :: Id |? Null
  , result :: Maybe Value
  , error :: Maybe ResponseError
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON ResponseMessage where
  toJSON ResponseMessage{id, result, error} =
    object $
      ["jsonrpc" .= ("2.0" :: Text)
      , "id" .= id
      ]
      ++ case result of { Nothing -> []; Just r -> ["result" .= r] }
      ++ case error of { Nothing -> []; Just e -> ["error" .= e] }

instance FromJSON ResponseMessage where
  parseJSON = withObject "ResponseMessage" $ \o -> do
    (jsonrpc :: Text) <- o .: "jsonrpc"
    id <- o .: "id"
    result <- o LSP..:!? "result"
    error <- o LSP..:!? "error"
    unless (jsonrpc == "2.0") $ fail "The LSP library only supports JSONRPC 2.0"
    pure $ ResponseMessage id result error

deriving via ViaJSON ResponseMessage instance Pretty ResponseMessage

data Message = Req RequestMessage | Rsp ResponseMessage | Not NotificationMessage
  deriving stock (Show, Eq, Ord)

deriveJSON jsonOptionsUntagged ''Message

deriving via ViaJSON Message instance Pretty Message
