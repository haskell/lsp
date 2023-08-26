{-# LANGUAGE RecordWildCards #-}

module Language.LSP.Server.Progress (
  new,
  progressHandlers,
  withProgress,
  withIndefiniteProgress,
  ProgressAmount (..),
  ProgressCancellable (..),
  ProgressCancelledException,
) where

import Colog.Core
import Control.Concurrent.Async
import Control.Concurrent.Extra as C
import Control.Concurrent.STM
import Control.Exception qualified as E
import Control.Lens hiding (Empty)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson qualified as J
import Data.Foldable
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import JSONRPC.Typed.Message
import JSONRPC.Typed.Method
import JSONRPC.Typed.RPC qualified as RPC
import JSONRPC.Typed.Server
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Message qualified as L
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as L
import Language.LSP.Server.Core
import UnliftIO qualified as U
import UnliftIO.Exception qualified as UE

{- | A package indicating the percentage of progress complete and a
 an optional message to go with it during a 'withProgress'.
-}
data ProgressAmount = ProgressAmount (Maybe UInt) (Maybe Text)

-- | Thrown if the user cancels a 'withProgress'/'withIndefiniteProgress'/ session.
data ProgressCancelledException = ProgressCancelledException
  deriving stock (Show)

instance E.Exception ProgressCancelledException

{- | Whether or not the user should be able to cancel a 'withProgress'/'withIndefiniteProgress'
 session.
-}
data ProgressCancellable = Cancellable | NotCancellable

new ::
  LogAction IO (WithSeverity LspProgressLog) ->
  Int ->
  Int ->
  ServerHandle Server L.Method ->
  L.ClientCapabilities ->
  IO ProgressHandle
new progressLogger progressStartDelay progressUpdateDelay serverHandle clientCapabilities = do
  progressNextId <- newTVarIO 0
  progressCancel <- newTVarIO mempty
  pure ProgressHandle{..}

-- Get a new id for the progress session and make a new one
getNewProgressId :: ProgressHandle -> STM ProgressToken
getNewProgressId pdata =
  stateTVar (pdata.progressNextId) $ \cur ->
    let !next = cur + 1
     in (L.ProgressToken $ L.InL cur, next)
{-# INLINE getNewProgressId #-}

cancelProgress ::
  ProgressHandle ->
  MethodParams L.Method_WindowWorkDoneProgressCancel ->
  IO ()
cancelProgress pdata (L.WorkDoneProgressCancelParams tid) = do
  let logger = pdata.progressLogger
  actions <- readTVarIO pdata.progressCancel
  case Map.lookup tid actions of
    Nothing -> return ()
    Just cancelAction -> do
      logger <& ProgressCancel tid `WithSeverity` Debug
      liftIO cancelAction

progressHandlers :: ProgressHandle -> Handlers Server L.Method
progressHandlers pdata =
  notificationHandler SMethod_WindowWorkDoneProgressCancel $
    mkNotificationHandler $
      cancelProgress pdata

---- User functions

withProgressBase ::
  ProgressHandle ->
  Bool ->
  Text ->
  Maybe ProgressToken ->
  ProgressCancellable ->
  ((ProgressAmount -> IO ()) -> IO a) ->
  IO a
withProgressBase handle indefinite title clientToken cancellable f = do
  let initialProgress = ProgressAmount (if indefinite then Nothing else Just 0) Nothing
  let ProgressHandle{progressStartDelay = startDelay, progressUpdateDelay = updateDelay} = handle

  tokenVar <- liftIO newEmptyTMVarIO
  reportVar <- liftIO $ newTMVarIO initialProgress
  endBarrier <- liftIO newEmptyMVar

  let
    updater :: ProgressAmount -> IO ()
    updater pa = atomically $ do
      -- I don't know of a way to do this with a normal MVar!
      -- That is: put something into it regardless of whether it is full or empty
      _ <- tryTakeTMVar reportVar
      putTMVar reportVar pa

    progressEnded :: IO ()
    progressEnded = readMVar endBarrier

    endProgress :: IO ()
    endProgress = void $ tryPutMVar endBarrier ()

    -- Once we have a 'ProgressToken', store it in the variable and also register the cancellation
    -- handler.
    registerToken :: ProgressToken -> IO ()
    registerToken t = atomically $ do
      putTMVar tokenVar t
      modifyTVar handle.progressCancel (Map.insert t endProgress)

    -- Deregister our 'ProgressToken', specifically its cancellation handler. It is important
    -- to do this reliably or else we will leak handlers.
    unregisterToken :: IO ()
    unregisterToken = atomically $ do
      mt <- tryReadTMVar tokenVar
      for_ mt $ \t -> modifyTVar handle.progressCancel (Map.delete t)

    -- Find and register our 'ProgressToken', asking the client for it if necessary.
    -- Note that this computation may terminate before we get the token, we need to wait
    -- for the token var to be filled if we want to use it.
    createToken :: IO ()
    createToken = do
      -- See Note [Delayed progress reporting]
      -- This delays the creation of the token as well as the 'begin' message. Creating
      -- the token shouldn't result in any visible action on the client side since
      -- the title/initial percentage aren't given until the 'begin' mesage. However,
      -- it's neater not to create tokens that we won't use, and clients may find it
      -- easier to clean them up if they receive begin/end reports for them.
      threadDelay startDelay
      case clientToken of
        -- See Note [Client- versus server-initiated progress]
        -- Client-initiated progress
        Just t -> registerToken t
        -- Try server-initiated progress
        Nothing -> do
          t <- atomically $ getNewProgressId handle

          let
            handler :: Either (ResponseError Method_WindowWorkDoneProgressCreate) (MethodResult Method_WindowWorkDoneProgressCreate) -> IO ()
            handler = \case
              -- Successfully registered the token, we can now use it.
              -- So we go ahead and start. We do this as soon as we get the
              -- token back so the client gets feedback ASAP
              Right _ -> registerToken t
              -- The client sent us an error, we can't use the token.
              Left _err -> pure ()
          -- If we don't have a progress token from the client and
          -- the client doesn't support server-initiated progress then
          -- there's nothing to do: we can't report progress.
          when (clientSupportsServerInitiatedProgress handle.clientCapabilities) $
            void $
              -- Server-initiated progress
              -- See Note [Client- versus server-initiated progress]
              sendRequest handle.serverHandle SMethod_WindowWorkDoneProgressCreate (WorkDoneProgressCreateParams t) $
                mkResponseHandler handler

    -- Actually send the progress reports.
    sendReports :: IO ()
    sendReports = do
      t <- atomically $ readTMVar tokenVar
      begin t
      -- Once we are sending updates, if we get interrupted we should send
      -- the end notification
      update t `UE.finally` end t
     where
      cancellable' = case cancellable of
        Cancellable -> Just True
        NotCancellable -> Just False
      begin t = do
        (ProgressAmount pct msg) <- liftIO $ atomically $ takeTMVar reportVar
        sendProgressReport t $ WorkDoneProgressBegin L.AString title cancellable' msg pct
      update t =
        forever $ do
          -- See Note [Delayed progress reporting]
          liftIO $ threadDelay updateDelay
          (ProgressAmount pct msg) <- liftIO $ atomically $ takeTMVar reportVar
          sendProgressReport t $ WorkDoneProgressReport L.AString Nothing msg pct
      end t = sendProgressReport t (WorkDoneProgressEnd L.AString Nothing)

    -- Create the token and then start sending reports; all of which races with the check for the
    -- progress having ended. In all cases, make sure to unregister the token at the end.
    progressThreads :: IO ()
    progressThreads =
      ((createToken >> sendReports) `UE.finally` unregisterToken) `U.race_` progressEnded

  withAsync (f updater) $ \mainAct ->
    -- If the progress gets cancelled then we need to get cancelled too
    withAsync progressThreads $ \pthreads -> do
      r <- waitEither mainAct pthreads
      -- TODO: is this weird? I can't see how else to gracefully use the ending barrier
      -- as a guard to cancel the other async
      case r of
        Left a -> pure a
        Right _ -> cancelWith mainAct ProgressCancelledException >> wait mainAct
 where
  sendProgressReport :: (J.ToJSON r) => ProgressToken -> r -> IO ()
  sendProgressReport token report =
    RPC.sendNotification handle.serverHandle.rpcHandle SMethod_Progress $ ProgressParams token $ J.toJSON report

clientSupportsServerInitiatedProgress :: L.ClientCapabilities -> Bool
clientSupportsServerInitiatedProgress caps = fromMaybe False $ caps ^? L.window . _Just . L.workDoneProgress . _Just
{-# INLINE clientSupportsServerInitiatedProgress #-}

{- |
Wrapper for reporting progress to the client during a long running task.
-}
withProgress ::
  ProgressHandle ->
  -- | The title of the progress operation
  Text ->
  -- | The progress token provided by the client in the method params, if any
  Maybe ProgressToken ->
  -- | Whether or not this operation is cancellable. If true, the user will be
  -- shown a button to allow cancellation. Note that requests can still be cancelled
  -- even if this is not set.
  ProgressCancellable ->
  -- | An update function to pass progress updates to
  ((ProgressAmount -> IO ()) -> IO a) ->
  IO a
withProgress h title clientToken cancellable f =
  withProgressBase h False title clientToken cancellable f

{- |
Same as 'withProgress', but for processes that do not report the precentage complete.
-}
withIndefiniteProgress ::
  ProgressHandle ->
  -- | The title of the progress operation
  Text ->
  -- | The progress token provided by the client in the method params, if any
  Maybe ProgressToken ->
  -- | Whether or not this operation is cancellable. If true, the user will be
  -- shown a button to allow cancellation. Note that requests can still be cancelled
  -- even if this is not set.
  ProgressCancellable ->
  -- | An update function to pass progress updates to
  ((Text -> IO ()) -> IO a) ->
  IO a
withIndefiniteProgress h title clientToken cancellable f = do
  withProgressBase h True title clientToken cancellable $ \update ->
      f $ \msg -> update (ProgressAmount Nothing (Just msg))

{- Note [Client- versus server-initiated progress]
The protocol supports both client- and server-initiated progress. Client-initiated progress
is simpler: the client gives you a progress token, and then you use that to report progress.
Server-initiated progress is more complex: you need to send a request to the client to tell
them about the token you want to use, and only after that can you send updates using it.
-}

{- Note [Delayed progress reporting]
Progress updates can be very noisy by default. There are two ways this can happen:
- Creating progress notifications for very short-lived operations that don't deserve them.
  This directs the user's attention to something that then immediately ceases to exist,
  which is annoying, the more so if it happens frequently.
- Very frequently updating progress information.

Now, in theory the client could deal with this for us. Probably they _should_: working
out how to display an (accurate) series of progress notifications from the server seems
like the client's job. Nonetheless, this does not always happen, and so it is helpful
to moderate the spam.

For this reason we have configurable delays on starting progress tracking and on sending
updates. However, the defaults are set to 0, so it's opt-in.
-}
