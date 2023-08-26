{-# LANGUAGE RecordWildCards #-}

module Language.LSP.Server.Config
  ( ConfigHandle
  , new
  , configHandlers
  , getConfig
  , setConfig
  , tryChangeConfig
  , requestConfigUpdate
  , updateFromInitializeParams
  , lookForConfigSection
  ) where

import Colog.Core
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson qualified as J
import Data.Foldable (for_)
import Data.Maybe
import Data.String
import Data.Text qualified as T
import JSONRPC.Typed.Method
import JSONRPC.Typed.Server
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server.Core
import Language.LSP.Server.DynamicRegistration qualified as Registration

new ::
  LogAction IO (WithSeverity LspConfigLog) ->
  ServerHandle Server LSP.Method ->
  RegistrationHandle ->
  LSP.ClientCapabilities ->
  T.Text ->
  config ->
  (config -> J.Value -> Either T.Text config) ->
  (config -> IO ()) ->
  IO (ConfigHandle config)
new logger serverHandle registrationHandle clientCapabilities configSection initialConfig parseConfig onConfigChange = do
  config <- newTVarIO initialConfig
  pure ConfigHandle{..}

{- | The current configuration from the client as set via the @initialize@ and
 @workspace/didChangeConfiguration@ requests, as well as by calls to
 'setConfig'.
-}
getConfig :: ConfigHandle config -> IO config
getConfig configHandle = readTVarIO configHandle.config

setConfig :: ConfigHandle config -> config -> IO ()
setConfig configHandle config = atomically $ writeTVar configHandle.config config

{- | Try to find the configuration section in an object that might represent "all" the settings.
 The heuristic we use is to look for a property with the right name, and use that if we find
 it. Otherwise we fall back to the whole object.
 See Note [LSP configuration]
-}
lookForConfigSection :: T.Text -> J.Value -> J.Value
lookForConfigSection section (J.Object o) | Just s' <- o ^. at (fromString $ T.unpack section) = s'
lookForConfigSection _ o = o

-- | Given a new config object, try to update our config with it.
tryChangeConfig :: ConfigHandle config -> J.Value -> IO ()
tryChangeConfig handle newConfigObject = do
  res <- atomically $ stateTVar handle.config $ \oldConfig ->
    case handle.parseConfig oldConfig newConfigObject of
      Left err -> (Left err, oldConfig)
      Right newConfig -> (Right newConfig, newConfig)
  case res of
    Left err -> do
      handle.logger <& ConfigurationParseError newConfigObject err `WithSeverity` Warning
    Right newConfig -> do
      handle.logger <& NewConfig newConfigObject `WithSeverity` Debug
      liftIO $ handle.onConfigChange newConfig

updateFromInitializeParams :: ConfigHandle config -> LSP.InitializeParams -> IO ()
updateFromInitializeParams handle p = do
  -- See Note [LSP configuration]
  let configObject = lookForConfigSection handle.configSection <$> (p ^. LSP.initializationOptions)
  for_ configObject $ tryChangeConfig handle

{- | Send a `worksapce/configuration` request to update the server's config.
Meth
 This is called automatically in response to `workspace/didChangeConfiguration` notifications
 from the client, so should not normally be called manually.
-}
requestConfigUpdate ::
  ConfigHandle config ->
  IO ()
requestConfigUpdate handle = do
  let supportsConfiguration = fromMaybe False $ handle.clientCapabilities ^? L.workspace . _Just . L.configuration . _Just
      section = handle.configSection
  if supportsConfiguration
    then do
      let params = LSP.ConfigurationParams [LSP.ConfigurationItem Nothing (Just section)]
      res <- requestAwaitResponse @Server @LSP.Method handle.serverHandle LSP.SMethod_WorkspaceConfiguration params
      case res of
        Right [newConfigObject] -> tryChangeConfig handle newConfigObject
        Right sections -> handle.logger <& WrongConfigSections sections `WithSeverity` Error
        Left err -> handle.logger <& BadConfigurationResponse err `WithSeverity` Error
    else handle.logger <& ConfigurationNotSupported `WithSeverity` Debug

-- | Handle a workspace/didChangeConfiguration request.
handleDidChangeConfiguration ::
  ConfigHandle config ->
  MethodParams LSP.Method_WorkspaceDidChangeConfiguration ->
  IO ()
handleDidChangeConfiguration handle params = do
  -- See Note [LSP configuration]

  -- There are a few cases:
  -- 1. Client supports `workspace/configuration` and sends nothing in `workspace/didChangeConfiguration`
  --    Then we will fail the first attempt and succeed the second one.
  -- 2. Client does not support `workspace/configuration` and sends updated config in `workspace/didChangeConfiguration`.
  --    Then we will succeed the first attempt and fail (or in fact do nothing in) the second one.
  -- 3. Client supports `workspace/configuration` and sends updated config in `workspace/didChangeConfiguration`.
  --    Then both will succeed, which is a bit redundant but not a big deal.
  tryChangeConfig handle (lookForConfigSection handle.configSection $ params ^. L.settings)
  requestConfigUpdate handle

dynamicRegisterConfigNotifications :: ConfigHandle config -> IO ()
dynamicRegisterConfigNotifications handle = do
  -- We need to register for `workspace/didChangeConfiguration` dynamically in order to
  -- ensure we receive notifications. See
  -- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_configuration
  -- https://github.com/microsoft/language-server-protocol/issues/1888
  -- Ignore the registration ID, we're never going to unregister this
  void $ Registration.registerCapability
      handle.registrationHandle
      LSP.SMethod_WorkspaceDidChangeConfiguration
      (LSP.DidChangeConfigurationRegistrationOptions (Just $ LSP.InL handle.configSection))
      $ notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $
          mkNotificationHandler $ handleDidChangeConfiguration handle

configHandlers ::
  ConfigHandle config ->
  Handlers Server LSP.Method
configHandlers handle =
  notificationHandler LSP.SMethod_Initialized $
    mkNotificationHandler $
      \_ -> dynamicRegisterConfigNotifications handle >> requestConfigUpdate handle

{- Note [LSP configuration]
LSP configuration is a huge mess.
- The configuration model of the client is not specified
- Many of the configuration messages are not specified in what they should return

In particular, configuration appears in three places:
1. The `initializationOptions` field of the `initialize` request.
  - The contents of this are unspecified. "User provided initialization options".
2. The `settings` field of the `workspace/didChangeConfiguration` notification.
  - The contents of this are unspecified. "The actual changed settings".
3. The `section` field of the response to the `workspace/configuration` request.
  - This at least says it should be the settings corresponding to the sections
    specified in the request.

It's very hard to know what to do here. In particular, the first two cases seem
like they could include arbitrary configuration from the client that might not
relate to you. How you locate "your" settings is unclear.

We are on firmer ground with case 3. Then at least it seems that we can pick
a configuration section, just always ask for that, and require clients to use
that for our settings. Furthermore, this is the method that is encouraged by the
specification designers: https://github.com/microsoft/language-server-protocol/issues/567#issuecomment-420589320.

For this reason we mostly try and rely on `workspace/configuration`. That means
three things:
- We require servers to give a specific configuration section for us to use
  when requesting configuration.
- We can try and make sense of `initializationOptions`, but regardless we should
  send a `workspace/configuration` request afterwards (in the handler for the
  `initialized` notification, which is the earliest we can send messages:
  https://github.com/microsoft/language-server-protocol/issues/567#issuecomment-953772465)
- We can try and make sense of `didChangeConfiguration`, but regardless we should
  send a `workspace/configuration` request afterwards

We do try to make sense of the first two cases also, especially because clients do
not have to support `workspace/configuration`! In practice,
many clients seem to follow the sensible approach laid out here:
https://github.com/microsoft/language-server-protocol/issues/972#issuecomment-626668243

To make this work, we try to be tolerant by using the following strategy.
When we receive a configuration object from any of the sources above, we first
check to see if it has a field corresponding to our configuration section. If it
does, then we assume that it our config and try to parse it. If it does not, we
try to parse the entire config object. This hopefully lets us handle a variety
of sensible cases where the client sends us mostly our config, either wrapped
in our section or not.
-}
