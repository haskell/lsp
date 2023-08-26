{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}

module Language.LSP.Protocol.Message.Parsing where

import Language.LSP.Protocol.Internal.Method
import Language.LSP.Protocol.Message.LspId
import Language.LSP.Protocol.Message.Meta
import Language.LSP.Protocol.Message.Method
import Language.LSP.Protocol.Message.Types

import Data.Aeson
import Data.Aeson.Types
import Data.Function (on)
import Data.GADT.Compare
import Data.Kind
import Data.Proxy
import Data.Type.Equality
import GHC.TypeLits (sameSymbol)

-- ---------------------------------------------------------------------
-- Working with arbitrary messages
-- ---------------------------------------------------------------------

data FromServerMessage' a where
  FromServerMess :: forall t (m :: Method ServerToClient t) a. SMethod m -> TMessage m -> FromServerMessage' a
  FromServerRsp :: forall (m :: Method ClientToServer Request) a. a m -> TResponseMessage m -> FromServerMessage' a

type FromServerMessage = FromServerMessage' SMethod

instance Eq FromServerMessage where
  (==) = (==) `on` toJSON
instance Show FromServerMessage where
  show = show . toJSON

instance ToJSON FromServerMessage where
  toJSON (FromServerMess m p) = serverMethodJSON m (toJSON p)
  toJSON (FromServerRsp m p) = clientResponseJSON m (toJSON p)

fromServerNot ::
  forall (m :: Method ServerToClient Notification).
  TMessage m ~ TNotificationMessage m =>
  TNotificationMessage m ->
  FromServerMessage
fromServerNot m@TNotificationMessage{_method = meth} = FromServerMess meth m

fromServerReq ::
  forall (m :: Method ServerToClient Request).
  TMessage m ~ TRequestMessage m =>
  TRequestMessage m ->
  FromServerMessage
fromServerReq m@TRequestMessage{_method = meth} = FromServerMess meth m

data FromClientMessage' a where
  FromClientMess :: forall t (m :: Method ClientToServer t) a. SMethod m -> TMessage m -> FromClientMessage' a
  FromClientRsp :: forall (m :: Method ServerToClient Request) a. a m -> TResponseMessage m -> FromClientMessage' a

type FromClientMessage = FromClientMessage' SMethod

instance ToJSON FromClientMessage where
  toJSON (FromClientMess m p) = clientMethodJSON m (toJSON p)
  toJSON (FromClientRsp m p) = serverResponseJSON m (toJSON p)

fromClientNot ::
  forall (m :: Method ClientToServer Notification).
  TMessage m ~ TNotificationMessage m =>
  TNotificationMessage m ->
  FromClientMessage
fromClientNot m@TNotificationMessage{_method = meth} = FromClientMess meth m

fromClientReq ::
  forall (m :: Method ClientToServer Request).
  TMessage m ~ TRequestMessage m =>
  TRequestMessage m ->
  FromClientMessage
fromClientReq m@TRequestMessage{_method = meth} = FromClientMess meth m

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
parseServerMessage :: LookupFunc ClientToServer a -> Value -> Parser (FromServerMessage' a)
parseServerMessage lookupId v@(Object o) = do
  methMaybe <- o .:! "method"
  idMaybe <- o .:! "id"
  case methMaybe of
    -- Request or Notification
    Just (SomeServerMethod m) ->
      case splitServerMethod m of
        IsServerNot -> FromServerMess m <$> parseJSON v
        IsServerReq -> FromServerMess m <$> parseJSON v
        IsServerEither | SMethod_CustomMethod (p :: Proxy s') <- m -> do
          case idMaybe of
            -- Request
            Just _ ->
              let m' = (SMethod_CustomMethod p :: SMethod (Method_CustomMethod s' :: Method ServerToClient Request))
               in FromServerMess m' <$> parseJSON v
            Nothing ->
              let m' = (SMethod_CustomMethod p :: SMethod (Method_CustomMethod s' :: Method ServerToClient Notification))
               in FromServerMess m' <$> parseJSON v
    Nothing -> do
      case idMaybe of
        Just i -> do
          case lookupId i of
            Just (m, res) -> clientResponseJSON m $ FromServerRsp res <$> parseJSON v
            Nothing -> fail $ unwords ["Failed in looking up response type of", show v]
        Nothing -> fail $ unwords ["Got unexpected message without method or id"]
parseServerMessage _ v = fail $ unwords ["parseServerMessage expected object, got:", show v]

{-# INLINE parseClientMessage #-}
parseClientMessage :: LookupFunc ServerToClient a -> Value -> Parser (FromClientMessage' a)
parseClientMessage lookupId v@(Object o) = do
  methMaybe <- o .:! "method"
  idMaybe <- o .:! "id"
  case methMaybe of
    -- Request or Notification
    Just (SomeClientMethod m) ->
      case splitClientMethod m of
        IsClientNot -> FromClientMess m <$> parseJSON v
        IsClientReq -> FromClientMess m <$> parseJSON v
        IsClientEither | SMethod_CustomMethod (p :: Proxy s') <- m -> do
          case idMaybe of
            -- Request
            Just _ ->
              let m' = (SMethod_CustomMethod p :: SMethod (Method_CustomMethod s' :: Method ClientToServer Request))
               in FromClientMess m' <$> parseJSON v
            Nothing ->
              let m' = (SMethod_CustomMethod p :: SMethod (Method_CustomMethod s' :: Method ClientToServer Notification))
               in FromClientMess m' <$> parseJSON v
    Nothing -> do
      case idMaybe of
        Just i -> do
          case lookupId i of
            Just (m, res) -> serverResponseJSON m $ FromClientRsp res <$> parseJSON v
            Nothing -> fail $ unwords ["Failed in looking up response type of", show v]
        Nothing -> fail $ unwords ["Got unexpected message without method or id"]
parseClientMessage _ v = fail $ unwords ["parseClientMessage expected object, got:", show v]

-- ---------------------------------------------------------------------
-- Helper Utilities
-- ---------------------------------------------------------------------

{-# INLINE clientResponseJSON #-}
clientResponseJSON :: SClientMethod m -> (HasJSON (TResponseMessage m) => x) -> x
clientResponseJSON m x = case splitClientMethod m of
  IsClientReq -> x
  IsClientEither -> x

{-# INLINE serverResponseJSON #-}
serverResponseJSON :: SServerMethod m -> (HasJSON (TResponseMessage m) => x) -> x
serverResponseJSON m x = case splitServerMethod m of
  IsServerReq -> x
  IsServerEither -> x

{-# INLINE clientMethodJSON #-}
clientMethodJSON :: SClientMethod m -> (ToJSON (TClientMessage m) => x) -> x
clientMethodJSON m x =
  case splitClientMethod m of
    IsClientNot -> x
    IsClientReq -> x
    IsClientEither -> x

{-# INLINE serverMethodJSON #-}
serverMethodJSON :: SServerMethod m -> (ToJSON (TServerMessage m) => x) -> x
serverMethodJSON m x =
  case splitServerMethod m of
    IsServerNot -> x
    IsServerReq -> x
    IsServerEither -> x

type HasJSON a = (ToJSON a, FromJSON a, Eq a)

-- Reify universal properties about Client/Server Messages

type ClientNotOrReq :: forall t. Method ClientToServer t -> Type
data ClientNotOrReq m where
  IsClientNot ::
    ( HasJSON (TClientMessage m)
    , TMessage m ~ TNotificationMessage m
    ) =>
    ClientNotOrReq (m :: Method ClientToServer Notification)
  IsClientReq ::
    forall (m :: Method ClientToServer Request).
    ( HasJSON (TClientMessage m)
    , HasJSON (TResponseMessage m)
    , TMessage m ~ TRequestMessage m
    ) =>
    ClientNotOrReq m
  IsClientEither ::
    ClientNotOrReq (Method_CustomMethod s)

type ServerNotOrReq :: forall t. Method ServerToClient t -> Type
data ServerNotOrReq m where
  IsServerNot ::
    ( HasJSON (TServerMessage m)
    , TMessage m ~ TNotificationMessage m
    ) =>
    ServerNotOrReq (m :: Method ServerToClient Notification)
  IsServerReq ::
    forall (m :: Method ServerToClient Request).
    ( HasJSON (TServerMessage m)
    , HasJSON (TResponseMessage m)
    , TMessage m ~ TRequestMessage m
    ) =>
    ServerNotOrReq m
  IsServerEither ::
    ServerNotOrReq (Method_CustomMethod s)

{-# INLINE splitClientMethod #-}
splitClientMethod :: SClientMethod m -> ClientNotOrReq m
splitClientMethod = \case
  SMethod_Initialize -> IsClientReq
  SMethod_Initialized -> IsClientNot
  SMethod_Shutdown -> IsClientReq
  SMethod_Exit -> IsClientNot
  SMethod_WorkspaceDidChangeWorkspaceFolders -> IsClientNot
  SMethod_WorkspaceDidChangeConfiguration -> IsClientNot
  SMethod_WorkspaceDidChangeWatchedFiles -> IsClientNot
  SMethod_WorkspaceSymbol -> IsClientReq
  SMethod_WorkspaceExecuteCommand -> IsClientReq
  SMethod_WindowWorkDoneProgressCancel -> IsClientNot
  SMethod_TextDocumentDidOpen -> IsClientNot
  SMethod_TextDocumentDidChange -> IsClientNot
  SMethod_TextDocumentWillSave -> IsClientNot
  SMethod_TextDocumentWillSaveWaitUntil -> IsClientReq
  SMethod_TextDocumentDidSave -> IsClientNot
  SMethod_TextDocumentDidClose -> IsClientNot
  SMethod_TextDocumentCompletion -> IsClientReq
  SMethod_TextDocumentHover -> IsClientReq
  SMethod_TextDocumentSignatureHelp -> IsClientReq
  SMethod_TextDocumentDeclaration -> IsClientReq
  SMethod_TextDocumentDefinition -> IsClientReq
  SMethod_TextDocumentTypeDefinition -> IsClientReq
  SMethod_TextDocumentImplementation -> IsClientReq
  SMethod_TextDocumentReferences -> IsClientReq
  SMethod_TextDocumentDocumentHighlight -> IsClientReq
  SMethod_TextDocumentDocumentSymbol -> IsClientReq
  SMethod_TextDocumentCodeAction -> IsClientReq
  SMethod_TextDocumentCodeLens -> IsClientReq
  SMethod_TextDocumentDocumentLink -> IsClientReq
  SMethod_TextDocumentDocumentColor -> IsClientReq
  SMethod_TextDocumentColorPresentation -> IsClientReq
  SMethod_TextDocumentFormatting -> IsClientReq
  SMethod_TextDocumentRangeFormatting -> IsClientReq
  SMethod_TextDocumentOnTypeFormatting -> IsClientReq
  SMethod_TextDocumentRename -> IsClientReq
  SMethod_TextDocumentPrepareRename -> IsClientReq
  SMethod_TextDocumentFoldingRange -> IsClientReq
  SMethod_TextDocumentSelectionRange -> IsClientReq
  SMethod_TextDocumentPrepareCallHierarchy -> IsClientReq
  SMethod_TextDocumentLinkedEditingRange -> IsClientReq
  SMethod_CallHierarchyIncomingCalls -> IsClientReq
  SMethod_CallHierarchyOutgoingCalls -> IsClientReq
  SMethod_TextDocumentSemanticTokensFull -> IsClientReq
  SMethod_TextDocumentSemanticTokensFullDelta -> IsClientReq
  SMethod_TextDocumentSemanticTokensRange -> IsClientReq
  SMethod_WorkspaceWillCreateFiles -> IsClientReq
  SMethod_WorkspaceWillDeleteFiles -> IsClientReq
  SMethod_WorkspaceWillRenameFiles -> IsClientReq
  SMethod_WorkspaceDidCreateFiles -> IsClientNot
  SMethod_WorkspaceDidDeleteFiles -> IsClientNot
  SMethod_WorkspaceDidRenameFiles -> IsClientNot
  SMethod_TextDocumentMoniker -> IsClientReq
  SMethod_TextDocumentPrepareTypeHierarchy -> IsClientReq
  SMethod_TypeHierarchySubtypes -> IsClientReq
  SMethod_TypeHierarchySupertypes -> IsClientReq
  SMethod_TextDocumentInlineValue -> IsClientReq
  SMethod_TextDocumentInlayHint -> IsClientReq
  SMethod_TextDocumentDiagnostic -> IsClientReq
  SMethod_WorkspaceDiagnostic -> IsClientReq
  SMethod_CodeLensResolve -> IsClientReq
  SMethod_InlayHintResolve -> IsClientReq
  SMethod_CodeActionResolve -> IsClientReq
  SMethod_DocumentLinkResolve -> IsClientReq
  SMethod_CompletionItemResolve -> IsClientReq
  SMethod_WorkspaceSymbolResolve -> IsClientReq
  SMethod_NotebookDocumentDidChange -> IsClientNot
  SMethod_NotebookDocumentDidClose -> IsClientNot
  SMethod_NotebookDocumentDidOpen -> IsClientNot
  SMethod_NotebookDocumentDidSave -> IsClientNot
  SMethod_SetTrace -> IsClientNot
  SMethod_Progress -> IsClientNot
  SMethod_CancelRequest -> IsClientNot
  (SMethod_CustomMethod _) -> IsClientEither

{-# INLINE splitServerMethod #-}
splitServerMethod :: SServerMethod m -> ServerNotOrReq m
splitServerMethod = \case
  SMethod_WindowShowMessage -> IsServerNot
  SMethod_WindowShowMessageRequest -> IsServerReq
  SMethod_WindowShowDocument -> IsServerReq
  SMethod_WindowLogMessage -> IsServerNot
  SMethod_WindowWorkDoneProgressCreate -> IsServerReq
  SMethod_Progress -> IsServerNot
  SMethod_TelemetryEvent -> IsServerNot
  SMethod_ClientRegisterCapability -> IsServerReq
  SMethod_ClientUnregisterCapability -> IsServerReq
  SMethod_WorkspaceWorkspaceFolders -> IsServerReq
  SMethod_WorkspaceConfiguration -> IsServerReq
  SMethod_WorkspaceApplyEdit -> IsServerReq
  SMethod_TextDocumentPublishDiagnostics -> IsServerNot
  SMethod_LogTrace -> IsServerNot
  SMethod_CancelRequest -> IsServerNot
  SMethod_WorkspaceCodeLensRefresh -> IsServerReq
  SMethod_WorkspaceSemanticTokensRefresh -> IsServerReq
  SMethod_WorkspaceInlineValueRefresh -> IsServerReq
  SMethod_WorkspaceInlayHintRefresh -> IsServerReq
  SMethod_WorkspaceDiagnosticRefresh -> IsServerReq
  (SMethod_CustomMethod _) -> IsServerEither

-- | Given a witness that two custom methods are of the same type, produce a witness that the methods are the same
data CustomEq m1 m2 where
  CustomEq ::
    (m1 ~ (Method_CustomMethod s :: Method f t1), m2 ~ (Method_CustomMethod s :: Method f t2)) =>
    {runCustomEq :: (t1 ~ t2 => m1 :~~: m2)} ->
    CustomEq m1 m2

runEq ::
  (t1 ~ t2) =>
  (SMethod m1 -> SMethod m2 -> Maybe (Either (CustomEq m1 m2) (m1 :~~: m2))) ->
  SMethod (m1 :: Method f t1) ->
  SMethod (m2 :: Method f t2) ->
  Maybe (m1 :~~: m2)
runEq f m1 m2 = do
  res <- f m1 m2
  pure $ case res of
    Right eq -> eq
    Left ceq -> runCustomEq ceq

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
    | SMethod_CustomMethod p1 <- m1
    , SMethod_CustomMethod p2 <- m2 =
        case sameSymbol p1 p2 of
          Just Refl -> Just $ Left $ CustomEq HRefl
          _ -> Nothing
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
    | SMethod_CustomMethod p1 <- m1
    , SMethod_CustomMethod p2 <- m2 =
        case sameSymbol p1 p2 of
          Just Refl -> Just $ Left $ CustomEq HRefl
          _ -> Nothing
  go _ _ = Nothing
