{-# LANGUAGE OverloadedStrings #-}

module Language.LSP.Test.Process (
  gracefullyWaitForProcess
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Retry
import Data.Maybe
import System.Exit
import UnliftIO.Process


-- Wait for a process to exit with a grace period.
-- This is more robust than using System.Process.waitForProcess combined with System.Timeout,
-- because waitForProcess may miss the async exception due to being in an FFI call.
gracefullyWaitForProcess :: (MonadLoggerIO m) => Int -> ProcessHandle -> m ()
gracefullyWaitForProcess gracePeriodUs p = do
  whenNothingM_ (waitForExit gracePeriodUs p) $ do
    logWarnN "Server process didn't stop after grace period; trying to interrupt"

    liftIO $ interruptProcessGroupOf p
    whenNothingM_ (waitForExit gracePeriodUs p) $ void $ do
      logWarnN "Server process didn't stop after a further grace period; going to terminate"
      liftIO $ terminateProcess p
      liftIO $ waitForExit gracePeriodUs p

waitForExit :: (MonadIO m) => Int -> ProcessHandle -> m (Maybe ExitCode)
waitForExit gracePeriodUs p = do
  liftIO $ retrying policy (\_ maybeExitCode -> return $ isNothing maybeExitCode)
                           (\_ -> getProcessExitCode p)
  where
    policy :: RetryPolicyM IO
    policy = limitRetriesByCumulativeDelay gracePeriodUs $ capDelay 1_000_000 (exponentialBackoff 50_000)

whenNothingM_ :: Monad m => m (Maybe a) -> m () -> m ()
whenNothingM_ mm action = mm >>= \m -> whenNothing_ m action

whenNothing_ :: Applicative f => Maybe a -> f () -> f ()
whenNothing_ Nothing m = m
whenNothing_ _       _ = pure ()
