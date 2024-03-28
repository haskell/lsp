{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Language.LSP.Server.Progress (
  withProgress,
  withIndefiniteProgress,
  ProgressAmount (..),
  ProgressCancellable (..),
  ProgressCancelledException,
) where

import Control.Concurrent.Async
import Control.Concurrent.Extra as C
import Control.Concurrent.STM
import Control.Exception qualified as E
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.Aeson qualified as J
import Data.Foldable
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as L
import Language.LSP.Server.Core
import UnliftIO qualified as U
import UnliftIO.Exception qualified as UE

{- | A package indicating the percentage of progress complete and a
 an optional message to go with it during a 'withProgress'

 @since 0.10.0.0
-}
data ProgressAmount = ProgressAmount (Maybe UInt) (Maybe Text)

{- | Thrown if the user cancels a 'Cancellable' 'withProgress'/'withIndefiniteProgress'/ session

 @since 0.11.0.0
-}
data ProgressCancelledException = ProgressCancelledException
  deriving (Show)

instance E.Exception ProgressCancelledException

{- | Whether or not the user should be able to cancel a 'withProgress'/'withIndefiniteProgress'
 session

 @since 0.11.0.0
-}
data ProgressCancellable = Cancellable | NotCancellable

-- Get a new id for the progress session and make a new one
getNewProgressId :: MonadLsp config m => m ProgressToken
getNewProgressId = do
  stateState (progressNextId . resProgressData) $ \cur ->
    let !next = cur + 1
     in (L.ProgressToken $ L.InL cur, next)
{-# INLINE getNewProgressId #-}

withProgressBase ::
  forall c m a.
  MonadLsp c m =>
  Bool ->
  Text ->
  Maybe ProgressToken ->
  ProgressCancellable ->
  ((ProgressAmount -> m ()) -> m a) ->
  m a
withProgressBase indefinite title clientToken cancellable f = do
  let initialProgress = ProgressAmount (if indefinite then Nothing else Just 0) Nothing
  LanguageContextEnv{resProgressStartDelay = startDelay, resProgressUpdateDelay = updateDelay} <- getLspEnv

  tokenVar <- liftIO newEmptyTMVarIO
  reportVar <- liftIO $ newTMVarIO initialProgress
  endBarrier <- liftIO newEmptyMVar

  let
    updater :: ProgressAmount -> m ()
    updater pa = liftIO $ atomically $ do
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
    registerToken :: ProgressToken -> m ()
    registerToken t = do
      handlers <- getProgressCancellationHandlers
      liftIO $ atomically $ do
        putTMVar tokenVar t
        modifyTVar handlers (Map.insert t endProgress)

    -- Deregister our 'ProgressToken', specifically its cancellation handler. It is important
    -- to do this reliably or else we will leak handlers.
    unregisterToken :: m ()
    unregisterToken = do
      handlers <- getProgressCancellationHandlers
      liftIO $ atomically $ do
        mt <- tryReadTMVar tokenVar
        for_ mt $ \t -> modifyTVar handlers (Map.delete t)

    -- Find and register our 'ProgressToken', asking the client for it if necessary.
    -- Note that this computation may terminate before we get the token, we need to wait
    -- for the token var to be filled if we want to use it.
    createToken :: m ()
    createToken = do
      -- See Note [Delayed progress reporting]
      -- This delays the creation of the token as well as the 'begin' message. Creating
      -- the token shouldn't result in any visible action on the client side since
      -- the title/initial percentage aren't given until the 'begin' mesage. However,
      -- it's neater not to create tokens that we won't use, and clients may find it
      -- easier to clean them up if they receive begin/end reports for them.
      liftIO $ threadDelay startDelay
      case clientToken of
        -- See Note [Client- versus server-initiated progress]
        -- Client-initiated progress
        Just t -> registerToken t
        -- Try server-initiated progress
        Nothing -> do
          t <- getNewProgressId
          clientCaps <- getClientCapabilities

          -- If we don't have a progress token from the client and
          -- the client doesn't support server-initiated progress then
          -- there's nothing to do: we can't report progress.
          when (clientSupportsServerInitiatedProgress clientCaps)
            $ void
            $
            -- Server-initiated progress
            -- See Note [Client- versus server-initiated progress]
            sendRequest
              SMethod_WindowWorkDoneProgressCreate
              (WorkDoneProgressCreateParams t)
            $ \case
              -- Successfully registered the token, we can now use it.
              -- So we go ahead and start. We do this as soon as we get the
              -- token back so the client gets feedback ASAP
              Right _ -> registerToken t
              -- The client sent us an error, we can't use the token.
              Left _err -> pure ()

    -- Actually send the progress reports.
    sendReports :: m ()
    sendReports = do
      t <- liftIO $ atomically $ readTMVar tokenVar
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
    progressThreads :: m ()
    progressThreads =
      ((createToken >> sendReports) `UE.finally` unregisterToken) `U.race_` liftIO progressEnded

  withRunInIO $ \runInBase -> do
    withAsync (runInBase $ f updater) $ \mainAct ->
      -- If the progress gets cancelled then we need to get cancelled too
      withAsync (runInBase progressThreads) $ \pthreads -> do
        r <- waitEither mainAct pthreads
        -- TODO: is this weird? I can't see how else to gracefully use the ending barrier
        -- as a guard to cancel the other async
        case r of
          Left a -> pure a
          Right _ -> cancelWith mainAct ProgressCancelledException >> wait mainAct
 where
  sendProgressReport :: (J.ToJSON r) => ProgressToken -> r -> m ()
  sendProgressReport token report = sendNotification SMethod_Progress $ ProgressParams token $ J.toJSON report

  getProgressCancellationHandlers :: m (TVar (Map.Map ProgressToken (IO ())))
  getProgressCancellationHandlers = getStateVar (progressCancel . resProgressData)

clientSupportsServerInitiatedProgress :: L.ClientCapabilities -> Bool
clientSupportsServerInitiatedProgress caps = fromMaybe False $ caps.window >>= \w -> w.workDoneProgress
{-# INLINE clientSupportsServerInitiatedProgress #-}

{- |
Wrapper for reporting progress to the client during a long running task.
-}
withProgress ::
  MonadLsp c m =>
  -- | The title of the progress operation
  Text ->
  -- | The progress token provided by the client in the method params, if any
  Maybe ProgressToken ->
  -- | Whether or not this operation is cancellable. If true, the user will be
  -- shown a button to allow cancellation. Note that requests can still be cancelled
  -- even if this is not set.
  ProgressCancellable ->
  -- | An update function to pass progress updates to
  ((ProgressAmount -> m ()) -> m a) ->
  m a
withProgress title clientToken cancellable f = withProgressBase False title clientToken cancellable f

{- |
Same as 'withProgress', but for processes that do not report the precentage complete.
-}
withIndefiniteProgress ::
  MonadLsp c m =>
  -- | The title of the progress operation
  Text ->
  -- | The progress token provided by the client in the method params, if any
  Maybe ProgressToken ->
  -- | Whether or not this operation is cancellable. If true, the user will be
  -- shown a button to allow cancellation. Note that requests can still be cancelled
  -- even if this is not set.
  ProgressCancellable ->
  -- | An update function to pass progress updates to
  ((Text -> m ()) -> m a) ->
  m a
withIndefiniteProgress title clientToken cancellable f =
  withProgressBase True title clientToken cancellable (\update -> f (\msg -> update (ProgressAmount Nothing (Just msg))))
