module JSONRPC.Typed.Message where

import Language.LSP.Protocol.Types (Null (..), type (|?) (..))
import Language.LSP.Protocol.Utils.Misc
import JSONRPC.Id
import JSONRPC.Message qualified as Untyped
import JSONRPC.Method qualified as Untyped
import JSONRPC.Typed.Method
import Control.Lens hiding ((.=), re)
import Control.Monad.Except (MonadError (throwError))
import Control.Applicative
import Data.Aeson hiding (Null)
import Data.Aeson qualified as J
import Data.Aeson.Types qualified as J
import Data.GADT.Compare
import Data.Generics.Labels ()
import Data.Int (Int32)
import Data.Maybe
import Data.Singletons
import Data.Some
import Data.Text (Text)
import GHC.Generics
import Prettyprinter

-- | Typed notification message, containing the correct parameter payload.
data NotificationMessage (m :: k) = NotificationMessage
  { method :: Sing m
  , params :: MethodParams m
  }
  deriving stock (Generic)

deriving stock instance (Eq (Sing m), Eq (MethodParams m)) => Eq (NotificationMessage m)
deriving stock instance (Show (Sing m), Show (MethodParams m)) => Show (NotificationMessage m)
deriving via (ViaJSON (NotificationMessage (m :: k))) instance Method k => Pretty (NotificationMessage m)

{- Note [Missing 'params']
The 'params' field on requrests and notificaoins may be omitted according to the
JSON-RPC spec, but that doesn't quite work the way we want with the generic aeson
instance. Even if the 'MethodParams' type family happens to resolve to a 'Maybe',
we handle it generically and so we end up asserting that it must be present.

We fix this in a slightly dumb way by just adding the field in if it is missing,
set to null (which parses correctly for those 'Maybe' parameters also).
-}

instance (Method k, SingI m) => FromJSON (NotificationMessage (m :: k)) where
  parseJSON v = do
    nm <- parseJSON v
    case fromUntypedNot nm >>= assertNotType (sing @m) of
      Right tmsg -> pure tmsg
      Left e -> fail e

instance (Method k) => ToJSON (NotificationMessage (m :: k)) where
  toJSON = toJSON . toUntypedNot

assertNotType :: forall k (m :: k). (Method k) => Sing m -> Some (NotificationMessage @k) -> Either String (NotificationMessage m)
assertNotType m sm =
  case sm of
    Some tmsg | Just Refl <- geq m (tmsg ^. #method) -> pure tmsg
    _ -> throwError $ "Wrong method, expecting: " ++ show (fromSing m)

fromUntypedNot :: forall k. (Method k) => Untyped.NotificationMessage -> Either String (Some (NotificationMessage @k))
fromUntypedNot msg = do
  let um = msg ^. #method
  case fromUntypedMethod @k um of
    Just meth -> case toSing meth of
      SomeSing sng -> Some <$> fromKnownUntypedNot sng msg
    Nothing -> throwError $ "Unrecognized method: " ++ show um

fromKnownUntypedNot :: forall k (m :: k). (Method k) => Sing m -> Untyped.NotificationMessage -> Either String (NotificationMessage m)
fromKnownUntypedNot sng (Untyped.NotificationMessage _ mParams) = do
  case mParams of
    Nothing -> throwError "Missing params"
    Just params -> bring @_ @IsJsonParts (Proxy @k) sng $
      case J.fromJSON params of
        Success a -> Right $ NotificationMessage sng a
        Error e -> throwError e

toUntypedNot :: forall k (m :: k). (Method k) => NotificationMessage m -> Untyped.NotificationMessage
toUntypedNot (NotificationMessage sng mParams) =
  bring @_ @IsJsonParts (Proxy @k) sng $
    Untyped.NotificationMessage (toUntypedMethod $ fromSing sng) (Just $ toJSON mParams)

-- deriving via ViaJSON (NotificationMessage m) instance (Method k, SingI (m :: k)) => Pretty (NotificationMessage m)

-- | Typed request message, containing the correct parameter payload.
data RequestMessage (m :: k) = RequestMessage
  { id :: Id
  , method :: Sing m
  , params :: MethodParams m
  }
  deriving stock (Generic)

deriving stock instance (Eq (Sing m), Eq (MethodParams m)) => Eq (RequestMessage m)
deriving stock instance (Show (Sing m), Show (MethodParams m)) => Show (RequestMessage m)
deriving via (ViaJSON (RequestMessage (m :: k))) instance Method k => Pretty (RequestMessage m)

instance (Method k, SingI m) => FromJSON (RequestMessage (m :: k)) where
  -- See Note [Missing 'params']
  parseJSON v = do
    rm <- parseJSON v
    case fromUntypedReq rm >>= assertReqType (sing @m) of
      Right tmsg -> pure tmsg
      Left e -> fail e

instance (Method k) => ToJSON (RequestMessage (m :: k)) where
  toJSON = toJSON . toUntypedReq

assertReqType :: forall k (m :: k). (Method k) => Sing m -> Some (RequestMessage @k) -> Either String (RequestMessage m)
assertReqType m sm =
  case sm of
    Some tmsg | Just Refl <- geq m (tmsg ^. #method) -> pure tmsg
    _ -> throwError $ "Wrong method, expecting: " ++ show (fromSing m)

fromUntypedReq :: forall k. (Method k) => Untyped.RequestMessage -> Either String (Some (RequestMessage @k))
fromUntypedReq msg =
  let um = msg ^. #method
   in case fromUntypedMethod @k um of
        Just meth -> withSomeSing meth $ \sng ->
          Some <$> fromKnownUntypedReq sng msg
        Nothing -> throwError $ "Unrecognized method: " ++ show um

fromKnownUntypedReq :: forall k (m :: k). (Method k) => Sing m -> Untyped.RequestMessage -> Either String (RequestMessage m)
fromKnownUntypedReq sng (Untyped.RequestMessage mid _ mParams) =
  case mParams of
    Nothing -> throwError "Missing params"
    Just params ->
      bring @_ @IsJsonParts (Proxy @k) sng $
        case J.fromJSON params of
          Success a -> Right $ RequestMessage mid sng a
          Error e -> throwError e

toUntypedReq :: forall k (m :: k). (Method k) => RequestMessage m -> Untyped.RequestMessage
toUntypedReq (RequestMessage mid sng mParams) =
  bring @_ @IsJsonParts (Proxy @k) sng $
    Untyped.RequestMessage mid (toUntypedMethod $ fromSing sng) (Just $ toJSON mParams)

-- deriving via ViaJSON (RequestMessage m) instance (Method k, SingI (m :: k)) => Pretty (RequestMessage m)

data ResponseError (m :: k) = ResponseError
  { code :: Int32
  , message :: Text
  , xdata :: Maybe (ErrorData m)
  }
  deriving stock (Generic)

deriving stock instance Eq (ErrorData m) => Eq (ResponseError m)
deriving stock instance Show (ErrorData m) => Show (ResponseError m)
deriving via (ViaJSON (ResponseError (m :: k))) instance (SingI m, Method k) => Pretty (ResponseError m)

instance (Method k, SingI m) => FromJSON (ResponseError (m :: k)) where
  parseJSON v = do
    re <- parseJSON v
    -- We don't have method information here, so we can't do a consistency
    -- check, we just assume the implicit method one is correct
    case fromKnownUntypedRspErr (sing @m) re of
      Right tmsg -> pure tmsg
      Left e -> fail e

instance (Method k, SingI m) => ToJSON (ResponseError (m :: k)) where
  toJSON = toJSON . toUntypedRspErr (sing @m)

fromKnownUntypedRspErr :: forall k (m :: k). (Method k) => Sing m -> Untyped.ResponseError -> Either String (ResponseError m)
fromKnownUntypedRspErr sng (Untyped.ResponseError code msg mDat) =
  bring @_ @IsJsonParts (Proxy @k) sng $
    -- FIXME: dodgy omitting nonsense
    let dat = J.fromJSON (fromMaybe J.Null mDat)
     in case dat of
          Success a -> Right $ ResponseError code msg a
          Error e -> throwError e

toUntypedRspErr :: forall k (m :: k). (Method k) => Sing m -> ResponseError m -> Untyped.ResponseError
toUntypedRspErr sng (ResponseError code msg dat) =
  bring @_ @IsJsonParts (Proxy @k) sng $
    let dat' = J.toJSON <$> dat
     in Untyped.ResponseError code msg dat'

-- deriving via ViaJSON (ResponseError m) instance (Method k, SingI (m :: k)) => Pretty (ResponseError m)

-- | A typed response message with a correct result payload.
data ResponseMessage (m :: k) = ResponseMessage
  { id :: Id
  , result :: Either (ResponseError m) (MethodResult m)
  }
  deriving stock (Generic)

deriving stock instance (Eq (MethodResult m), Eq (ErrorData m)) => Eq (ResponseMessage m)
deriving stock instance (Show (MethodResult m), Show (ErrorData m)) => Show (ResponseMessage m)
deriving via (ViaJSON (ResponseMessage (m :: k))) instance (SingI m, Method k) => Pretty (ResponseMessage m)

instance (Method k, SingI m) => FromJSON (ResponseMessage (m :: k)) where
  parseJSON v = do
    rm <- parseJSON v
    -- We don't have method information here, so we can't do a consistency
    -- check, we just assume the implicit method one is correct
    case fromKnownUntypedRsp (sing @m) rm of
      Right tmsg -> pure tmsg
      Left e -> fail e

instance (Method k, SingI m) => ToJSON (ResponseMessage (m :: k)) where
  toJSON = toJSON . toUntypedRsp (sing @m)

fromUntypedRsp :: forall k. (Method k) => Untyped.Method -> Untyped.ResponseMessage -> Either String (Some (ResponseMessage @k))
fromUntypedRsp um msg = do
  case fromUntypedMethod @k um of
    Just meth -> withSomeSing meth $ \sng ->
      Some <$> fromKnownUntypedRsp sng msg
    Nothing -> throwError $ "Unrecognized method: " ++ show um

fromKnownUntypedRsp :: forall k (m :: k). Method k => Sing m -> Untyped.ResponseMessage -> Either String (ResponseMessage m)
fromKnownUntypedRsp _ (Untyped.ResponseMessage (InR Null) _ _) = Left "id is null"
fromKnownUntypedRsp sng (Untyped.ResponseMessage (InL mid) mRes mErr) =
  bring @_ @IsJsonParts (Proxy @k) sng $
    case (mRes, mErr) of
      (Just res, Nothing) ->
        case J.fromJSON res of
          Success a -> Right $ ResponseMessage mid (Right a)
          Error e -> throwError e
      (Nothing, Just err) -> do
        err' <- fromKnownUntypedRspErr sng err
        pure $ ResponseMessage mid (Left err')
      (Just err, Just res) -> throwError $ "both error and result cannot be present: " ++ show err ++ "," ++ show res
      (Nothing, Nothing) -> throwError "both error and result cannot be missing"

toUntypedRsp :: forall k (m :: k). (Method k) => Sing m -> ResponseMessage m -> Untyped.ResponseMessage
toUntypedRsp sng (ResponseMessage mid r) =
  bring @_ @IsJsonParts (Proxy @k) sng $
    let (res, err) = case r of
          Right v -> (Just $ toJSON v, Nothing)
          Left e -> (Nothing, Just $ toUntypedRspErr sng e)
     in Untyped.ResponseMessage (InL mid) res err

-- deriving via ViaJSON (ResponseMessage m) instance (Method k, SingI (m :: k)) => Pretty (ResponseMessage m)

-- ---------------------------------------------------------------------
-- Combined message types
-- ---------------------------------------------------------------------

-- TODO: is this really useful?
data Message (r :: Role) k (m :: k) where
  Not :: (NotificationOk r m) => Sing m -> NotificationMessage m -> Message r k m
  Req :: (RequestOk r m) => Sing m -> RequestMessage m -> Message r k m
  Rsp :: (ResponseOk r m) => Sing m -> ResponseMessage m -> Message r k m

deriving stock instance (Eq (Sing m), Eq (MethodParams m), Eq (MethodResult m), Eq (ErrorData m)) => Eq (Message r k m)
deriving stock instance (Show (Sing m), Show (MethodParams m), Show (MethodResult m), Show (ErrorData m)) => Show (Message r k m)

instance (Method k, SingI m, SingI r) => FromJSON (Message r k (m :: k)) where
  parseJSON = withObject "Message" $ \o -> do
    let ms = sing @m
        rs = sing @r
        ors = sOtherRole rs
        is = sMethodInitiator ms
    case (sMethodType ms, sCanInitiate rs is, sCanInitiate ors is) of
      (SNotification, SYes, _) -> Not ms <$> parseJSON (Object o)
      (SRequest, SYes, _) -> Req ms <$> parseJSON (Object o)
      (SRequest, _, SYes) -> Rsp ms <$> parseJSON (Object o)
      -- TODO: more error
      _ -> fail $ "got message that can't be sent: " ++ show (fromSing ms)

instance (Method k) => ToJSON (Message r k (m :: k)) where
  toJSON = \case
    Not ms msg -> withSingI ms $ toJSON msg
    Req ms msg -> withSingI ms $ toJSON msg
    Rsp ms msg -> withSingI ms $ toJSON msg

data SomeMessage r k where
  SomeMessage :: forall r k (m :: k). Message r k m -> SomeMessage r k

instance (Method k) => Show (SomeMessage r k) where
  show (SomeMessage msg) = show $ encode msg

instance (Method k) => Pretty (SomeMessage r k) where
  pretty (SomeMessage msg) = prettyJSON $ toJSON msg

instance (Method k) => ToJSON (SomeMessage r k) where
  toJSON (SomeMessage msg) = toJSON msg

parseSomeMessage :: forall r k . (Method k, SingI r) => (Id -> Maybe Untyped.Method) -> J.Value -> J.Parser (SomeMessage r k)
parseSomeMessage lookupId = withObject "Message" $ \o -> do
  methMaybe <- o J..:! "method"
  idMaybe <- o J..:! "id"
  let bestGuessMethod = methMaybe <|> (idMaybe >>= lookupId)
  case bestGuessMethod >>= fromUntypedMethod of
    Just m -> withSomeSing @k m  $ \sm -> parser sm (J.Object o)
    Nothing -> fail "Could not determine method for message"
  where
    parser :: forall (m :: k) . (Method k, SingI r) => Sing m -> J.Value -> J.Parser (SomeMessage r k)
    parser sm v = withSingI sm $ SomeMessage <$> J.parseJSON @(Message r k m) v
