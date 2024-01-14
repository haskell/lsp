{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeInType #-}

module Language.LSP.Protocol.Message.Types where

import Language.LSP.Protocol.Internal.Method
import Language.LSP.Protocol.Message.LspId
import Language.LSP.Protocol.Message.Meta
import Language.LSP.Protocol.Message.Method ()
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Utils.Misc

import Data.Aeson hiding (Null)
import Data.Aeson qualified as J
import Data.Aeson.TH
import Data.Kind
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits (KnownSymbol)
import Prettyprinter

-- 'RequestMessage', 'ResponseMessage', 'ResponseError', and 'NotificationMessage'
-- aren't present in the metamodel, although they should be.
-- https://github.com/microsoft/vscode-languageserver-node/issues/1079

-- | Notification message type as defined in the spec.
data NotificationMessage = NotificationMessage
  { _jsonrpc :: Text
  , _method :: Text
  , _params :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

deriveJSON lspOptions ''NotificationMessage
deriving via ViaJSON NotificationMessage instance Pretty NotificationMessage

-- This isn't present in the metamodel.

-- | Request message type as defined in the spec.
data RequestMessage = RequestMessage
  { _jsonrpc :: Text
  , _id :: Int32 |? Text
  , _method :: Text
  , _params :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

deriveJSON lspOptions ''RequestMessage
deriving via ViaJSON RequestMessage instance Pretty RequestMessage

-- | Response error type as defined in the spec.
data ResponseError = ResponseError
  { _code :: LSPErrorCodes |? ErrorCodes
  , _message :: Text
  , _xdata :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

{- Note [ErrorCodes and LSPErrorCodes]

Confusingly, the metamodel defines _two_ enums for error codes. One of
these covers JSON RPC errors and one covers LSP-specific errors. We want
to accept either, mostly so we can make use of the pre-specified enum values.

However, _both_ of them are listed as accepting custom values. This means
that `LSPErrorCodes |? ErrorCodes` isn't quite right: when we parse it from
JSON, if we get an error code that isn't a known value of `LSPErrorCodes`, we
will just use the custom value constructor, without trying `ErrorCodes`.

It's hard to find any other good way of representing things properly with what
we've got, so in the end we decided to patch up the JSON parsing with a custom
instance.
-}
deriveToJSON lspOptions ''ResponseError
instance FromJSON ResponseError where
  parseJSON =
    let errorCode = withObject "ResponseError" $ \v ->
          ResponseError
            <$> v .: "code"
            <*> v .: "message"
            <*> v .:!? "data"
     in fmap go . errorCode
   where
    go :: ResponseError -> ResponseError
    go x@(ResponseError (InL (LSPErrorCodes_Custom n)) _ _) =
      x{_code = InR (fromOpenEnumBaseType n)}
    go x = x

deriving via ViaJSON ResponseError instance Pretty ResponseError

-- | Response message type as defined in the spec.
data ResponseMessage = ResponseMessage
  { _jsonrpc :: Text
  , _id :: Int32 |? Text |? Null
  , _result :: Maybe Value
  , _error :: Maybe ResponseError
  }
  deriving stock (Show, Eq, Generic)

deriveJSON lspOptions ''ResponseMessage

deriving via ViaJSON ResponseMessage instance Pretty ResponseMessage

-----

-- | Typed notification message, containing the correct parameter payload.
data TNotificationMessage (m :: Method f Notification) = TNotificationMessage
  { _jsonrpc :: Text
  , _method :: SMethod m
  , _params :: MessageParams m
  }
  deriving stock (Generic)

deriving stock instance Eq (MessageParams m) => Eq (TNotificationMessage m)
deriving stock instance Show (MessageParams m) => Show (TNotificationMessage m)

{- Note [Missing 'params']
The 'params' field on requrests and notificaoins may be omitted according to the
JSON-RPC spec, but that doesn't quite work the way we want with the generic aeson
instance. Even if the 'MessageParams' type family happens to resolve to a 'Maybe',
we handle it generically and so we end up asserting that it must be present.

We fix this in a slightly dumb way by just adding the field in if it is missing,
set to null (which parses correctly for those 'Maybe' parameters also).
-}

instance (FromJSON (MessageParams m), FromJSON (SMethod m)) => FromJSON (TNotificationMessage m) where
  -- See Note [Missing 'params']
  parseJSON = genericParseJSON lspOptions . addNullField "params"
instance (ToJSON (MessageParams m)) => ToJSON (TNotificationMessage m) where
  toJSON = genericToJSON lspOptions
  toEncoding = genericToEncoding lspOptions

deriving via ViaJSON (TNotificationMessage m) instance (ToJSON (MessageParams m)) => Pretty (TNotificationMessage m)

-- | Typed request message, containing the correct parameter payload.
data TRequestMessage (m :: Method f Request) = TRequestMessage
  { _jsonrpc :: Text
  , _id :: LspId m
  , _method :: SMethod m
  , _params :: MessageParams m
  }
  deriving stock (Generic)

deriving stock instance Eq (MessageParams m) => Eq (TRequestMessage m)
deriving stock instance Show (MessageParams m) => Show (TRequestMessage m)

instance (FromJSON (MessageParams m), FromJSON (SMethod m)) => FromJSON (TRequestMessage m) where
  -- See Note [Missing 'params']
  parseJSON = genericParseJSON lspOptions . addNullField "params"
instance (ToJSON (MessageParams m)) => ToJSON (TRequestMessage m) where
  toJSON = genericToJSON lspOptions
  toEncoding = genericToEncoding lspOptions

deriving via ViaJSON (TRequestMessage m) instance (ToJSON (MessageParams m)) => Pretty (TRequestMessage m)

data TResponseError (m :: Method f Request) = TResponseError
  { _code :: LSPErrorCodes |? ErrorCodes
  , _message :: Text
  , _xdata :: Maybe (ErrorData m)
  }
  deriving stock (Generic)

deriving stock instance Eq (ErrorData m) => Eq (TResponseError m)
deriving stock instance Show (ErrorData m) => Show (TResponseError m)

instance (FromJSON (ErrorData m)) => FromJSON (TResponseError m) where
  parseJSON =
    let errorCode = withObject "ResponseError" $ \v ->
          TResponseError
            <$> v .: "code"
            <*> v .: "message"
            <*> v .:!? "data"
     in fmap go . errorCode
   where
    go :: TResponseError m -> TResponseError m
    go x@(TResponseError (InL (LSPErrorCodes_Custom n)) _ _) =
      x{_code = InR (fromOpenEnumBaseType n)}
    go x = x
instance (ToJSON (ErrorData m)) => ToJSON (TResponseError m) where
  toJSON = genericToJSON lspOptions
  toEncoding = genericToEncoding lspOptions

deriving via ViaJSON (TResponseError m) instance (ToJSON (ErrorData m)) => Pretty (TResponseError m)

-- TODO: similar functions for the others?
toUntypedResponseError :: (ToJSON (ErrorData m)) => TResponseError m -> ResponseError
toUntypedResponseError (TResponseError c m d) = ResponseError c m (fmap toJSON d)

-- | A typed response message with a correct result payload.
data TResponseMessage (m :: Method f Request) = TResponseMessage
  { _jsonrpc :: Text
  , _id :: Maybe (LspId m)
  , -- TODO: use `TResponseError m` for the error type, this will require quite a lot of adaptation downstream
    _result :: Either ResponseError (MessageResult m)
  }
  deriving stock (Generic)

deriving stock instance (Eq (MessageResult m), Eq (ErrorData m)) => Eq (TResponseMessage m)
deriving stock instance (Show (MessageResult m), Show (ErrorData m)) => Show (TResponseMessage m)

instance (ToJSON (MessageResult m), ToJSON (ErrorData m)) => ToJSON (TResponseMessage m) where
  toJSON TResponseMessage{_jsonrpc = jsonrpc, _id = lspid, _result = result} =
    object
      [ "jsonrpc" .= jsonrpc
      , "id" .= lspid
      , case result of
          Left err -> "error" .= err
          Right a -> "result" .= a
      ]

instance (FromJSON (MessageResult a), FromJSON (ErrorData a)) => FromJSON (TResponseMessage a) where
  parseJSON = withObject "Response" $ \o -> do
    _jsonrpc <- o .: "jsonrpc"
    _id <- o .: "id"
    _result <- o .:!? "result"
    _error <- o .:!? "error"
    result <- case (_error, _result) of
      (Just err, Nothing) -> pure $ Left err
      (Nothing, Just res) -> pure $ Right res
      (Just _err, Just _res) -> fail $ "both error and result cannot be present: " ++ show o
      (Nothing, Nothing) -> fail "both error and result cannot be Nothing"
    return $ TResponseMessage _jsonrpc _id result

deriving via ViaJSON (TResponseMessage m) instance (ToJSON (MessageResult m), ToJSON (ErrorData m)) => Pretty (TResponseMessage m)

{- | A typed custom message. A special data type is needed to distinguish between
 notifications and requests, since a CustomMethod can be both!
-}
data TCustomMessage s f t where
  ReqMess :: TRequestMessage (Method_CustomMethod s :: Method f Request) -> TCustomMessage s f Request
  NotMess :: TNotificationMessage (Method_CustomMethod s :: Method f Notification) -> TCustomMessage s f Notification

deriving stock instance Show (TCustomMessage s f t)

instance ToJSON (TCustomMessage s f t) where
  toJSON (ReqMess a) = toJSON a
  toJSON (NotMess a) = toJSON a

instance KnownSymbol s => FromJSON (TCustomMessage s f Request) where
  parseJSON v = ReqMess <$> parseJSON v
instance KnownSymbol s => FromJSON (TCustomMessage s f Notification) where
  parseJSON v = NotMess <$> parseJSON v

deriving via ViaJSON (TCustomMessage s f t) instance (KnownSymbol s) => Pretty (TCustomMessage s f t)

-- ---------------------------------------------------------------------
-- Helper Type Families
-- ---------------------------------------------------------------------

{- | Map a method to the Request/Notification type with the correct
 payload.
-}
type TMessage :: forall f t. Method f t -> Type
type family TMessage m where
  TMessage (Method_CustomMethod s :: Method f t) = TCustomMessage s f t
  TMessage (m :: Method f Request) = TRequestMessage m
  TMessage (m :: Method f Notification) = TNotificationMessage m

-- Some helpful type synonyms
type TClientMessage (m :: Method ClientToServer t) = TMessage m
type TServerMessage (m :: Method ServerToClient t) = TMessage m

{- | Replace a missing field in an object with a null field, to simplify parsing
 This is a hack to allow other types than Maybe to work like Maybe in allowing the field to be missing.
 See also this issue: https://github.com/haskell/aeson/issues/646
-}
addNullField :: String -> Value -> Value
addNullField s (Object o) = Object $ o <> fromString s .= J.Null
addNullField _ v = v
