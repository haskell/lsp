module Json where

import Data.Aeson

import Data.Singletons
import JSONRPC.Id
import JSONRPC.Typed.Message
import JSONRPC.Typed.Method
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import TestLib

aesonGolden :: (ToJSON a) => String -> a -> TestTree
aesonGolden name a = goldenVsString (name ++ " (golden)") ("jsonrpc-typed-test/golden/" ++ name ++ ".json") (pure $ encode a)

aesonRoundtrip :: (ToJSON a, FromJSON a, Eq a, Show a) => String -> a -> TestTree
aesonRoundtrip name a = testCase (name ++ " (roundtrip)") $ eitherDecode (encode a) @?= Right a

propSomeMessageRoundtrip :: forall (r :: Role). Sing r -> TestTree
propSomeMessageRoundtrip rs = withSingI rs $
  testProperty ("SomeMessage (" ++ show (fromSing rs) ++ ") roundtrip") $
    property $ \(m :: SomeMessage r TestMethod) ->
      case m of SomeMessage msg -> messageRoundtrip msg

messageRoundtrip :: forall r k (m :: k). (Method k, Eq (Sing m)) => Message r k m -> Bool
messageRoundtrip = \case
  Req s msg -> withSingI s $ bring @_ @EqParts (Proxy @k) s $ eitherDecode (encode msg) == Right msg
  Not s msg -> withSingI s $ bring @_ @EqParts (Proxy @k) s $ eitherDecode (encode msg) == Right msg
  Rsp s msg -> withSingI s $ bring @_ @EqParts (Proxy @k) s $ eitherDecode (encode msg) == Right msg

test_json :: TestTree
test_json =
  let
    rm1 = RequestMessage (IdInt 1) SM1 1
    nm1 = NotificationMessage SM2 5
    rsm1 = ResponseMessage @_ @M3 (IdInt 2) (Right 1)
   in
    testGroup
      "encoding/decoding"
      [ aesonGolden "RequestMessage1" rm1
      , aesonGolden "NotificationMessage1" nm1
      , withSingI SM3 $ aesonGolden "ResponseMessage1" rsm1
      , propSomeMessageRoundtrip SServer
      , propSomeMessageRoundtrip SClient
      -- TODO: parse some literal JSON files including ones missing parameters
      ]
