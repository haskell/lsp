{-# LANGUAGE RecordWildCards #-}

module Language.LSP.Server.DynamicRegistration
  ( RegistrationHandle
  , new
  , registerCapability
  , unregisterCapability
  ) where

import Colog.Core
import Control.Concurrent.STM
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Singletons
import Data.Text qualified as T
import JSONRPC.Typed.Method
import JSONRPC.Typed.Server
import JSONRPC.Server qualified as Untyped
import Language.LSP.Protocol.Capabilities (dynamicRegistrationSupported)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server.Core
import Control.Monad

new ::
  LogAction IO (WithSeverity LspRegistrationLog) ->
  ServerHandle Server LSP.Method ->
  LSP.ClientCapabilities ->
  IO RegistrationHandle
new logger serverHandle clientCapabilities = do
  registrationMethods <- newTVarIO mempty
  nextRegistrationId <- newTVarIO 0
  pure $ RegistrationHandle{..}

freshRegistrationId :: RegistrationHandle -> IO RegistrationId
freshRegistrationId handle =
  atomically $ stateTVar handle.nextRegistrationId $ \cur ->
    let !next = cur + 1
     in (RegistrationId $ T.pack $ show cur, next)

-- TODO: notification registration!
{- | Sends a @client/registerCapability@ request and dynamically registers
 a 'Method' with a 'Handler'. Returns 'Nothing' if the client does not
 support dynamic registration for the specified method, otherwise a
 'RegistrationToken' which can be used to unregister it later.
-}
registerCapability ::
  RegistrationHandle ->
  LSP.SMethod m ->
  LSP.RegistrationOptions m ->
  Handlers Server LSP.Method ->
  IO (Maybe RegistrationId)
registerCapability handle method regOpts newHandlers = do
  existingHandlers <- readTVarIO handle.serverHandle.untypedServerHandle.handlers
  let attemptedReRegistrations =
        Untyped.handledMethods existingHandlers
        `Set.intersection`
        Untyped.handledMethods (untypedHandlers newHandlers)
  unless (Set.null attemptedReRegistrations) $
    handle.logger <& HandlersAlreadyExist method attemptedReRegistrations `WithSeverity` Warning
  atomically $ setHandlers handle.serverHandle newHandlers
  mtoken <- sendRegistration handle method regOpts
  case mtoken of
    Just rid -> do
      atomically $ modifyTVar handle.registrationMethods (Map.insert rid (fromSing method))
      pure $ Just rid
    -- If we get here then we already logged a warning in 'trySendRegistration'
    -- but arguably we should revert the changes we did here...
    Nothing -> pure Nothing

sendRegistration ::
  RegistrationHandle ->
  LSP.SMethod m ->
  LSP.RegistrationOptions m ->
  IO (Maybe RegistrationId)
sendRegistration handle method regOpts = do
  -- First, check to see if the client supports dynamic registration on this method
  if dynamicRegistrationSupported method handle.clientCapabilities
    then do
      rid@(RegistrationId ridTxt) <- freshRegistrationId handle
      let registration = LSP.TRegistration ridTxt method (Just regOpts)
          params = LSP.RegistrationParams [toUntypedRegistration registration]

      res <- requestAwaitResponse handle.serverHandle LSP.SMethod_ClientRegisterCapability params
      case res of
        Right _rsp -> pure $ Just rid
        Left err -> do
          handle.logger <& RegistrationFailed err `WithSeverity` Error
          pure Nothing
    else do
      handle.logger <& RegistrationUnsupported method `WithSeverity` Error
      pure Nothing

{- | Sends a @client/unregisterCapability@ request and removes the handler
 for that associated registration.
-}
unregisterCapability :: RegistrationHandle -> RegistrationId -> IO ()
unregisterCapability handle rid = do
  mmeth <- atomically $ do
    meths <- readTVar handle.registrationMethods
    let mmeth = Map.lookup rid meths
    case mmeth of
      Just meth -> do
        modifyTVar handle.registrationMethods (Map.delete rid)
        pure $ Just meth
      Nothing -> pure Nothing
  case mmeth of
    Just meth -> sendUnregistration handle meth rid
    Nothing -> handle.logger <& NoRegistration rid `WithSeverity` Error

sendUnregistration :: RegistrationHandle -> LSP.Method -> RegistrationId ->  IO ()
sendUnregistration handle meth (RegistrationId ridText) = do
  let unregistration = LSP.Unregistration ridText (T.pack $ LSP.methodToMethodString meth)
      params = LSP.UnregistrationParams [unregistration]
  res <- requestAwaitResponse handle.serverHandle LSP.SMethod_ClientUnregisterCapability params
  case res of
    Right _rsp -> pure ()
    Left err -> handle.logger <& UnregistrationFailed err `WithSeverity` Error
