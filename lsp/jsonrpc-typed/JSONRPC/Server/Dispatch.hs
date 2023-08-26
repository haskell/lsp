module JSONRPC.Server.Dispatch where

import Colog.Core (
  LogAction (..),
  Severity (..),
  WithSeverity (..),
  hoistLogAction,
  (<&),
 )

import Control.Concurrent.STM
import Control.Exception (bracket_)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Except ()
import Control.Monad.IO.Class
import Data.Aeson.Lens ()
import Data.Generics.Labels ()
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import JSONRPC.Id
import JSONRPC.Message
import JSONRPC.Method
import JSONRPC.RPC qualified as RPC
import JSONRPC.Server.Core
import Language.LSP.Protocol.Types (Null (..), type (|?) (..))
import Prettyprinter
import Prelude hiding (id)

data DispatchLog
  = MissingHandler Method
  | MissingResponseHandler Id [Id]
  | MissingResponseId Message
  | WrongResponseHandlerMethod Method Id Method
  | HandlingMessage Message
  | SuccessfulRequest Message
  | SuccessfulResponse Message
  | SuccessfulNotification Message
  | StartingDispatching
  | StoppingDispatching
  deriving stock (Show)

instance Pretty DispatchLog where
  pretty (MissingHandler m) = "No handler for:" <+> viaShow m
  pretty (MissingResponseId m) = "Response with no ID: " <+> viaShow m
  pretty (MissingResponseHandler i ids) =
    vsep
      [ "No response handler for id:" <+> pretty i
      , "Known ids:" <+> pretty ids
      ]
  pretty (WrongResponseHandlerMethod m1 i m2) =
    "Response handler for id "
      <+> pretty i
      <+> "expected response with method"
      <+> viaShow m1
      <+> "but got"
      <+> viaShow m2
  pretty (HandlingMessage m) = "Handling message:" <+> pretty m
  pretty (SuccessfulRequest m) = "Successfully handled request:" <+> pretty m
  pretty (SuccessfulResponse m) = "Successfully handled response:" <+> pretty m
  pretty (SuccessfulNotification m) = "Successfully handled notification:" <+> pretty m
  pretty StartingDispatching = "Starting dispatching thread"
  pretty StoppingDispatching = "Stopping dispatching thread"

dispatchLoop ::
  LogAction IO (WithSeverity DispatchLog) ->
  ServerHandle ->
  RPC.Connection ->
  IO ()
dispatchLoop logger handle c =
  bracket_
    (logger <& StartingDispatching `WithSeverity` Debug)
    (logger <& StoppingDispatching `WithSeverity` Debug)
    $ forever
    $ do
      msg <- c ^. #recvMessage
      logger <& HandlingMessage msg `WithSeverity` Debug
      action <- atomically $ handleMessage (hoistLogAction liftIO logger) handle msg
      action

handleMessage ::
  LogAction IO (WithSeverity DispatchLog) ->
  ServerHandle ->
  Message ->
  STM (IO ())
handleMessage logger serverHandle smsg = do
  Handlers{reqHandlers, notHandlers} <- readTVar (handlers serverHandle)
  let rpcH = serverHandle.rpcHandle
  let pendingResponsesVar = serverHandle ^. #pendingResponses
  pending <- readTVar pendingResponsesVar

  case smsg of
    Req msg ->
      let meth = msg ^. #method
       in case Map.lookup meth reqHandlers of
            Just h -> pure $ do
              res <- handleRequest h msg
              RPC.sendMsg rpcH $ Rsp res
              logger <& SuccessfulRequest smsg `WithSeverity` Debug
            Nothing -> pure $ do
              logger <& MissingHandler meth `WithSeverity` Warning
              let errorMsg = T.pack $ unwords ["No handler for: ", show meth]
                  err = ResponseError (-32601) errorMsg Nothing
              RPC.sendResponse rpcH (msg ^. #method) (msg ^. #id) (Left err)
    Not msg ->
      let meth = msg ^. #method
       in case Map.lookup meth notHandlers of
            Just h -> pure $ do
              handleNotification h msg
              logger <& SuccessfulNotification smsg `WithSeverity` Debug
            Nothing -> pure $ logger <& MissingHandler meth `WithSeverity` Warning
    Rsp msg ->
      case msg ^. #id of
        InL (rid :: Id) -> case Map.lookup rid pending of
          Just h -> do
            let newMap = Map.delete rid pending
            writeTVar pendingResponsesVar newMap
            pure $ do
              handleResponse h msg
              logger <& SuccessfulResponse smsg `WithSeverity` Debug
          Nothing -> pure $ logger <& MissingResponseHandler rid (Map.keys pending) `WithSeverity` Error
        -- TODO: log the error that we got sent if any
        InR Null -> pure $ logger <& MissingResponseId smsg `WithSeverity` Error
