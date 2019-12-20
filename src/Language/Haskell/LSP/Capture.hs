{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.LSP.Capture where

import Data.Aeson
import Data.ByteString.Lazy.Char8 as BSL
import Data.Time.Clock
import GHC.Generics
import Language.Haskell.LSP.Types

data Event = FromClient UTCTime FromServerMessage
           | FromServer UTCTime FromClientMessage
  deriving (Show, Eq, Generic, ToJSON)
captureFromClient = undefined
captureFromServer = undefined

{-
captureFromServer :: RequestMap -> Value -> Maybe FilePath -> IO ()
captureFromServer _ Nothing = return ()
captureFromServer msg (Just fp) = do
  time <- getCurrentTime
  let entry = FromServer time $ toJSON msg
  BSL.appendFile fp $ BSL.append (encode entry) "\n"

captureFromClient :: Value -> Maybe FilePath -> IO ()
captureFromClient _ Nothing = return ()
captureFromClient msg (Just fp) = do
  time <- getCurrentTime
  let entry = FromClient time $ toJSON msg
  BSL.appendFile fp $ BSL.append (encode entry) "\n"

matchResponseMsgType :: SomeClientMethod -> B.ByteString -> FromServerMessage
matchResponseMsgType (SomeClientMethod m) x = case splitClientMethod m of
    IsClientReq -> FromServerRsp m (fromJust $ decode x)
    IsClientEither -> FromServerRsp m (fromJust $ decode x)
    IsClientNot -> error "Got response to notification"

decodeFromServerMsg :: RequestMap -> B.ByteString -> FromServerMessage
decodeFromServerMsg reqMap bytes =
  case HM.lookup "method" obj of
    Just methodStr -> case fromJSON methodStr of
      Success (SomeServerMethod SWorkspaceWorkspaceFolders) -> error "ReqWorkspaceFolders not supported yet"
      Success (SomeServerMethod SWorkspaceConfiguration   ) -> error "ReqWorkspaceConfiguration not supported yet"
      Success (SomeServerMethod m) -> FromServerMess m $ fromJust $ serverMethodJSON m $ decode bytes
      Error e -> error e

    Nothing -> case decode bytes :: Maybe (ResponseMessage Value) of
      Just msg -> case HM.lookup (requestId $ msg ^. id) reqMap of
        Just req -> matchResponseMsgType req bytes -- try to decode it to more specific type
        Nothing  -> error "Couldn't match up response with request"
      Nothing -> error "Couldn't decode message"
    where obj = fromJust $ decode bytes :: Object
          -}
