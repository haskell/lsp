{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TupleSections              #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Language.LSP.Types.Parsing where

import           Language.LSP.Types.LspId
import           Language.LSP.Types.Method
import           Language.LSP.Types.Message
import qualified Data.HashMap.Strict as HM

import Data.Aeson
import Data.Aeson.Types
import Data.GADT.Compare
import Data.Type.Equality
import Data.Function (on)

-- ---------------------------------------------------------------------
-- Working with arbritary messages
-- ---------------------------------------------------------------------

data FromServerMessage' a where
  FromServerMess :: forall t (m :: Method FromServer t) a. SMethod m -> Message m -> FromServerMessage' a
  FromServerRsp  :: forall (m :: Method FromClient Request) a. a m -> ResponseMessage m -> FromServerMessage' a

type FromServerMessage = FromServerMessage' SMethod

instance Eq FromServerMessage where
  (==) = (==) `on` toJSON
instance Show FromServerMessage where
  show = show . toJSON

instance ToJSON FromServerMessage where
  toJSON (FromServerMess m p) = serverMethodJSON m (toJSON p)
  toJSON (FromServerRsp m p) = clientResponseJSON m (toJSON p)

fromServerNot :: forall (m :: Method FromServer Notification).
  Message m ~ NotificationMessage m => NotificationMessage m -> FromServerMessage
fromServerNot m@NotificationMessage{_method=meth} = FromServerMess meth m

fromServerReq :: forall (m :: Method FromServer Request).
  Message m ~ RequestMessage m => RequestMessage m -> FromServerMessage
fromServerReq m@RequestMessage{_method=meth} = FromServerMess meth m

data FromClientMessage' a where
  FromClientMess :: forall t (m :: Method FromClient t) a. SMethod m -> Message m -> FromClientMessage' a
  FromClientRsp  :: forall (m :: Method FromServer Request) a. a m -> ResponseMessage m -> FromClientMessage' a

type FromClientMessage = FromClientMessage' SMethod

instance ToJSON FromClientMessage where
  toJSON (FromClientMess m p) = clientMethodJSON m (toJSON p)
  toJSON (FromClientRsp m p) = serverResponseJSON m (toJSON p)

fromClientNot :: forall (m :: Method FromClient Notification).
  Message m ~ NotificationMessage m => NotificationMessage m -> FromClientMessage
fromClientNot m@NotificationMessage{_method=meth} = FromClientMess meth m

fromClientReq :: forall (m :: Method FromClient Request).
  Message m ~ RequestMessage m => RequestMessage m -> FromClientMessage
fromClientReq m@RequestMessage{_method=meth} = FromClientMess meth m

-- ---------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------

type LookupFunc f a = forall (m :: Method f Request). LspId m -> Maybe (SMethod m, a m)

{-
Message Types we must handle are the following

Request      | jsonrpc | id | method | params?
Response     | jsonrpc | id |        |         | response? | error?
Notification | jsonrpc |    | method | params?
-}

{-# INLINE parseServerMessage #-}
parseServerMessage :: LookupFunc FromClient a -> Value -> Parser (FromServerMessage' a)
parseServerMessage lookupId v@(Object o) = do
  case HM.lookup "method" o of
    Just cmd -> do
      -- Request or Notification
      SomeServerMethod m <- parseJSON cmd
      case splitServerMethod m of
        IsServerNot -> FromServerMess m <$> parseJSON v
        IsServerReq -> FromServerMess m <$> parseJSON v
        IsServerEither
          | HM.member "id" o -- Request
          , SCustomMethod cm <- m ->
              let m' = (SCustomMethod cm :: SMethod (CustomMethod :: Method FromServer Request))
                  in FromServerMess m' <$> parseJSON v
          | SCustomMethod cm <- m ->
              let m' = (SCustomMethod cm :: SMethod (CustomMethod :: Method FromServer Notification))
                  in FromServerMess m' <$> parseJSON v
    Nothing -> do
      case HM.lookup "id" o of
        Just i' -> do
          i <- parseJSON i'
          case lookupId i of
            Just (m,res) -> clientResponseJSON m $ FromServerRsp res <$> parseJSON v
            Nothing -> fail $ unwords ["Failed in looking up response type of", show v]
        Nothing -> fail $ unwords ["Got unexpected message without method or id"]
parseServerMessage _ v = fail $ unwords ["parseServerMessage expected object, got:",show v]

{-# INLINE parseClientMessage #-}
parseClientMessage :: LookupFunc FromServer a -> Value -> Parser (FromClientMessage' a)
parseClientMessage lookupId v@(Object o) = do
  case HM.lookup "method" o of
    Just cmd -> do
      -- Request or Notification
      SomeClientMethod m <- parseJSON cmd
      case splitClientMethod m of
        IsClientNot -> FromClientMess m <$> parseJSON v
        IsClientReq -> FromClientMess m <$> parseJSON v
        IsClientEither
          | HM.member "id" o -- Request
          , SCustomMethod cm <- m ->
              let m' = (SCustomMethod cm :: SMethod (CustomMethod :: Method FromClient Request))
                  in FromClientMess m' <$> parseJSON v
          | SCustomMethod cm <- m ->
              let m' = (SCustomMethod cm :: SMethod (CustomMethod :: Method FromClient Notification))
                  in FromClientMess m' <$> parseJSON v
    Nothing -> do
      case HM.lookup "id" o of
        Just i' -> do
          i <- parseJSON i'
          case lookupId i of
            Just (m,res) -> serverResponseJSON m $ FromClientRsp res <$> parseJSON v
            Nothing -> fail $ unwords ["Failed in looking up response type of", show v]
        Nothing -> fail $ unwords ["Got unexpected message without method or id"]
parseClientMessage _ v = fail $ unwords ["parseClientMessage expected object, got:",show v]

-- ---------------------------------------------------------------------
-- Helper Utilities
-- ---------------------------------------------------------------------

{-# INLINE clientResponseJSON #-}
clientResponseJSON :: SClientMethod m -> (HasJSON (ResponseMessage m) => x) -> x
clientResponseJSON m x = case splitClientMethod m of
  IsClientReq -> x
  IsClientEither -> x

{-# INLINE serverResponseJSON #-}
serverResponseJSON :: SServerMethod m -> (HasJSON (ResponseMessage m) => x) -> x
serverResponseJSON m x = case splitServerMethod m of
  IsServerReq -> x
  IsServerEither -> x

{-# INLINE clientMethodJSON#-}
clientMethodJSON :: SClientMethod m -> (ToJSON (ClientMessage m) => x) -> x
clientMethodJSON m x =
  case splitClientMethod m of
    IsClientNot -> x
    IsClientReq -> x
    IsClientEither -> x

{-# INLINE serverMethodJSON #-}
serverMethodJSON :: SServerMethod m -> (ToJSON (ServerMessage m) => x) -> x
serverMethodJSON m x =
  case splitServerMethod m of
    IsServerNot -> x
    IsServerReq -> x
    IsServerEither -> x

type HasJSON a = (ToJSON a,FromJSON a,Eq a)

-- Reify universal properties about Client/Server Messages

data ClientNotOrReq (m :: Method FromClient t) where
  IsClientNot
    :: ( HasJSON (ClientMessage m)
       , Message m ~ NotificationMessage m)
    => ClientNotOrReq (m :: Method FromClient Notification)
  IsClientReq
    :: forall (m :: Method FromClient Request).
    ( HasJSON (ClientMessage m)
    , HasJSON (ResponseMessage m)
    , Message m ~ RequestMessage m)
    => ClientNotOrReq m
  IsClientEither
    :: ClientNotOrReq CustomMethod

data ServerNotOrReq (m :: Method FromServer t) where
  IsServerNot
    :: ( HasJSON (ServerMessage m)
       , Message m ~ NotificationMessage m)
    => ServerNotOrReq (m :: Method FromServer Notification)
  IsServerReq
    :: forall (m :: Method FromServer Request).
    ( HasJSON (ServerMessage m)
    , HasJSON (ResponseMessage m)
    , Message m ~ RequestMessage m)
    => ServerNotOrReq m
  IsServerEither
    :: ServerNotOrReq CustomMethod

{-# INLINE splitClientMethod #-}
splitClientMethod :: SClientMethod m -> ClientNotOrReq m
splitClientMethod SInitialize = IsClientReq
splitClientMethod SInitialized = IsClientNot
splitClientMethod SShutdown = IsClientReq
splitClientMethod SExit = IsClientNot
splitClientMethod SWorkspaceDidChangeWorkspaceFolders = IsClientNot
splitClientMethod SWorkspaceDidChangeConfiguration = IsClientNot
splitClientMethod SWorkspaceDidChangeWatchedFiles = IsClientNot
splitClientMethod SWorkspaceSymbol = IsClientReq
splitClientMethod SWorkspaceExecuteCommand = IsClientReq
splitClientMethod SWindowWorkDoneProgressCancel = IsClientNot
splitClientMethod STextDocumentDidOpen = IsClientNot
splitClientMethod STextDocumentDidChange = IsClientNot
splitClientMethod STextDocumentWillSave = IsClientNot
splitClientMethod STextDocumentWillSaveWaitUntil = IsClientReq
splitClientMethod STextDocumentDidSave = IsClientNot
splitClientMethod STextDocumentDidClose = IsClientNot
splitClientMethod STextDocumentCompletion = IsClientReq
splitClientMethod SCompletionItemResolve = IsClientReq
splitClientMethod STextDocumentHover = IsClientReq
splitClientMethod STextDocumentSignatureHelp = IsClientReq
splitClientMethod STextDocumentDeclaration = IsClientReq
splitClientMethod STextDocumentDefinition = IsClientReq
splitClientMethod STextDocumentTypeDefinition = IsClientReq
splitClientMethod STextDocumentImplementation = IsClientReq
splitClientMethod STextDocumentReferences = IsClientReq
splitClientMethod STextDocumentDocumentHighlight = IsClientReq
splitClientMethod STextDocumentDocumentSymbol = IsClientReq
splitClientMethod STextDocumentCodeAction = IsClientReq
splitClientMethod STextDocumentCodeLens = IsClientReq
splitClientMethod SCodeLensResolve = IsClientReq
splitClientMethod STextDocumentDocumentLink = IsClientReq
splitClientMethod SDocumentLinkResolve = IsClientReq
splitClientMethod STextDocumentDocumentColor = IsClientReq
splitClientMethod STextDocumentColorPresentation = IsClientReq
splitClientMethod STextDocumentFormatting = IsClientReq
splitClientMethod STextDocumentRangeFormatting = IsClientReq
splitClientMethod STextDocumentOnTypeFormatting = IsClientReq
splitClientMethod STextDocumentRename = IsClientReq
splitClientMethod STextDocumentPrepareRename = IsClientReq
splitClientMethod STextDocumentFoldingRange = IsClientReq
splitClientMethod STextDocumentSelectionRange = IsClientReq
splitClientMethod SCancelRequest = IsClientNot
splitClientMethod SCustomMethod{} = IsClientEither

{-# INLINE splitServerMethod #-}
splitServerMethod :: SServerMethod m -> ServerNotOrReq m
splitServerMethod SWindowShowMessage = IsServerNot
splitServerMethod SWindowShowMessageRequest = IsServerReq
splitServerMethod SWindowLogMessage = IsServerNot
splitServerMethod SWindowWorkDoneProgressCreate = IsServerReq
splitServerMethod SProgress = IsServerNot
splitServerMethod STelemetryEvent = IsServerNot
splitServerMethod SClientRegisterCapability = IsServerReq
splitServerMethod SClientUnregisterCapability = IsServerReq
splitServerMethod SWorkspaceWorkspaceFolders = IsServerReq
splitServerMethod SWorkspaceConfiguration = IsServerReq
splitServerMethod SWorkspaceApplyEdit = IsServerReq
splitServerMethod STextDocumentPublishDiagnostics = IsServerNot
splitServerMethod SCancelRequest = IsServerNot
splitServerMethod SCustomMethod{} = IsServerEither

-- | Given a witness that two custom methods are of the same type, produce a witness that the methods are the same
data CustomEq m1 m2 where
  CustomEq
    :: (m1 ~ CustomMethod, m2 ~ CustomMethod)
    => (t1 :~: t2 -> m1 :~~: m2)
    -> CustomEq (m1 :: Method f t1) (m2 :: Method f t2)

applyCustomEq :: (t1 :~: t2) -> CustomEq (m1 :: Method f t1) (m2 :: Method f t2) -> m1 :~~: m2
applyCustomEq x (CustomEq f) = f x

runEq :: (t1 ~ t2)
      => (SMethod m1 -> SMethod m2 -> Maybe (Either (CustomEq m1 m2) (m1 :~~: m2)))
      -> SMethod (m1 :: Method f t1)
      -> SMethod (m2 :: Method f t2)
      -> Maybe (m1 :~~: m2)
runEq f m1 m2 = do
  res <- f m1 m2
  pure $ case res of
    Right eq -> eq
    Left ceq -> applyCustomEq Refl ceq

-- | Heterogeneous equality on singleton server methods
mEqServer :: SServerMethod m1 -> SServerMethod m2 -> Maybe (Either (CustomEq m1 m2) (m1 :~~: m2))
mEqServer m1 m2 = go (splitServerMethod m1) (splitServerMethod m2)
  where
    go IsServerNot IsServerNot = do
      Refl <- geq m1 m2
      pure $ Right HRefl
    go IsServerReq IsServerReq = do
      Refl <- geq m1 m2
      pure $ Right HRefl
    go IsServerEither IsServerEither
      | SCustomMethod c1 <- m1
      , SCustomMethod c2 <- m2
      , c1 == c2
      = Just $ Left $ CustomEq $ \Refl -> HRefl
    go _ _ = Nothing

-- | Heterogeneous equality on singleton client methods
mEqClient :: SClientMethod m1 -> SClientMethod m2 -> Maybe (Either (CustomEq m1 m2) (m1 :~~: m2))
mEqClient m1 m2 = go (splitClientMethod m1) (splitClientMethod m2)
  where
    go IsClientNot IsClientNot = do
      Refl <- geq m1 m2
      pure $ Right HRefl
    go IsClientReq IsClientReq = do
      Refl <- geq m1 m2
      pure $ Right HRefl
    go IsClientEither IsClientEither
      | SCustomMethod c1 <- m1
      , SCustomMethod c2 <- m2
      , c1 == c2
      = Just $ Left $ CustomEq $ \Refl -> HRefl
    go _ _ = Nothing
