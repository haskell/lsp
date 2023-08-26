{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestLib where

import Colog.Core
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Aeson
import Data.Foldable
import Data.GADT.Compare.TH
import Data.Maybe
import Data.Singletons
import Data.Singletons.TH qualified as TH
import Data.String
import Data.Text qualified as T
import GHC.Generics (Generic)
import JSONRPC.Id
import JSONRPC.Typed.Message
import JSONRPC.Typed.Method
import System.Console.Concurrent
import Test.QuickCheck

logStderrConcurrent :: MonadIO m => LogAction m String
logStderrConcurrent = LogAction $ \s -> liftIO $ errorConcurrent s

logMVar :: MonadIO m => m (MVar [a], LogAction m a)
logMVar = liftIO $ do
  var <- newMVar []
  pure (var, LogAction $ \s -> liftIO $ modifyMVar_ var (\logs -> pure (s : logs)))

printMVarLogs :: (Show a, MonadIO m) => MVar [a] -> m ()
printMVarLogs v = liftIO $ do
  logs <- readMVar v
  for_ (reverse logs) print

TH.genDefunSymbols [''Allowed, ''RequestOrNotification, ''Initiator]
TH.singletons
  [d|
    data TestMethod = M1 | M2 | M3 | M4 | M5 | M6

    tmMethodInitiator :: TestMethod -> Initiator
    tmMethodInitiator M1 = ClientInitiates
    tmMethodInitiator M2 = ClientInitiates
    tmMethodInitiator M3 = ServerInitiates
    tmMethodInitiator M4 = ServerInitiates
    tmMethodInitiator M5 = ClientInitiates
    tmMethodInitiator M6 = ClientInitiates

    tmMethodType :: TestMethod -> RequestOrNotification
    tmMethodType M1 = Request
    tmMethodType M2 = Notification
    tmMethodType M3 = Request
    tmMethodType M4 = Notification
    tmMethodType M5 = Request
    tmMethodType M6 = Request
    |]

deriving stock instance Show TestMethod
deriving stock instance Eq TestMethod
deriving stock instance Generic TestMethod
deriving stock instance Enum TestMethod
deriving stock instance Bounded TestMethod
deriving anyclass instance ToJSON TestMethod
deriving anyclass instance FromJSON TestMethod

deriving stock instance Eq (STestMethod m)
deriving stock instance Show (STestMethod m)

deriveGEq ''STestMethod
deriveGCompare ''STestMethod

deriveClosed ''TestMethod ''STestMethod

-------------------------

instance Method TestMethod where
  toUntypedMethod = \case
    M1 -> "M1"
    M2 -> "M2"
    M3 -> "M3"
    M4 -> "M4"
    M5 -> "M5"
    M6 -> "M6"

  fromUntypedMethod = \case
    "M1" -> Just M1
    "M2" -> Just M2
    "M3" -> Just M3
    "M4" -> Just M4
    "M5" -> Just M5
    "M6" -> Just M6
    _ -> Nothing

  type MethodInitiator m = TmMethodInitiator m
  sMethodInitiator = sTmMethodInitiator

  type MethodType m = TmMethodType m
  sMethodType = sTmMethodType

  type MethodParams M1 = Integer
  type MethodParams M2 = Integer
  type MethodParams M3 = Integer
  type MethodParams M4 = Integer
  type MethodParams M5 = Integer
  type MethodParams M6 = Integer

  type MethodResult M1 = Integer
  type MethodResult M2 = ()
  type MethodResult M3 = Integer
  type MethodResult M4 = ()
  type MethodResult M5 = Integer
  type MethodResult M6 = Integer

  type ErrorData M1 = ()
  type ErrorData M2 = ()
  type ErrorData M3 = ()
  type ErrorData M4 = ()
  type ErrorData M5 = ()
  type ErrorData M6 = ()

instance Arbitrary Id where
  arbitrary = oneof [IdInt <$> arbitrary, IdString . T.pack <$> arbitrary]

class (Arbitrary (MethodParams m), Arbitrary (MethodResult m), Arbitrary (ErrorData m)) => ArbitraryParts m
instance (Arbitrary (MethodParams m), Arbitrary (MethodResult m), Arbitrary (ErrorData m)) => ArbitraryParts m

instance (Closed k, k `Everywhere` ArbitraryParts, SingI m) => Arbitrary (RequestMessage (m :: k)) where
  arbitrary =
    bring @_ @ArbitraryParts (Proxy @k) (sing @m) $
      RequestMessage <$> arbitrary <*> pure (sing @m) <*> arbitrary

instance (Closed k, k `Everywhere` ArbitraryParts, SingI m) => Arbitrary (NotificationMessage (m :: k)) where
  arbitrary =
    bring @_ @ArbitraryParts (Proxy @k) (sing @m) $
      NotificationMessage <$> pure (sing @m) <*> arbitrary

instance (Closed k, k `Everywhere` ArbitraryParts, SingI m) => Arbitrary (ResponseError (m :: k)) where
  arbitrary =
    bring @_ @ArbitraryParts (Proxy @k) (sing @m) $
      ResponseError <$> arbitrary <*> (fromString <$> arbitrary) <*> arbitrary

instance (Closed k, k `Everywhere` ArbitraryParts, SingI m) => Arbitrary (ResponseMessage (m :: k)) where
  arbitrary =
    bring @_ @ArbitraryParts (Proxy @k) (sing @m) $
      ResponseMessage <$> arbitrary <*> arbitrary

instance (Method k, Closed k, k `Everywhere` ArbitraryParts, SingI r, Enum k, Bounded k) => Arbitrary (SomeMessage r k) where
  arbitrary = do
    let ms :: [k] = [minBound .. maxBound]
    let gs :: [Gen (SomeMessage r k)] = mapMaybe genMsgFor ms
    oneof gs
   where
    genMsgFor :: k -> Maybe (Gen (SomeMessage r k))
    genMsgFor m =
      case toSing m of
        SomeSing sng ->
          withSingI sng $
            let
              rsng = sing @r
              orsng = sOtherRole rsng
              is = sMethodInitiator sng
             in
              case (sMethodType sng, sCanInitiate rsng is, sCanInitiate orsng is) of
                (SRequest, SYes, _) -> Just $ SomeMessage . Req sng <$> arbitrary
                (SNotification, SYes, _) -> Just $ SomeMessage . Not sng <$> arbitrary
                (SRequest, _, SYes) -> Just $ SomeMessage . Rsp sng <$> arbitrary
                _ -> Nothing
