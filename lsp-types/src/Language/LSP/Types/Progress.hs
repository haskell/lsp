{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}

module Language.LSP.Types.Progress where

import           Control.Monad (unless)
import qualified Data.Aeson as A
import           Data.Aeson.TH
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import           Language.LSP.Types.Utils

-- | A token used to report progress back or return partial results for a
-- specific request.
-- @since 0.17.0.0
data ProgressToken
    = ProgressNumericToken Int
    | ProgressTextToken Text
    deriving (Show, Read, Eq, Ord)

deriveJSON lspOptionsUntagged ''ProgressToken

-- | Parameters for a $/progress notification.
data ProgressParams t =
    ProgressParams {
      _token :: ProgressToken
    , _value :: t
    } deriving (Show, Read, Eq, Functor)

deriveJSON lspOptions ''ProgressParams

-- | Parameters for 'WorkDoneProgressBeginNotification'.
--
-- @since 0.10.0.0
data WorkDoneProgressBeginParams =
  WorkDoneProgressBeginParams {
  -- | Mandatory title of the progress operation.
  -- Used to briefly inform about the kind of operation being
  -- performed. Examples: "Indexing" or "Linking dependencies".
   _title :: Text
  -- | Controls if a cancel button should show to allow the user to cancel the
  -- long running operation. Clients that don't support cancellation are allowed
  -- to ignore the setting.
  , _cancellable :: Maybe Bool
  -- | Optional, more detailed associated progress
  -- message. Contains complementary information to the
  -- '_title'. Examples: "3/25 files",
  -- "project/src/module2", "node_modules/some_dep". If
  -- unset, the previous progress message (if any) is
  -- still valid.
  , _message :: Maybe Text
  -- | Optional progress percentage to display (value 100 is considered 100%).
  -- If not provided infinite progress is assumed and clients are allowed
  -- to ignore the '_percentage' value in subsequent in report notifications.
  --
  -- The value should be steadily rising. Clients are free to ignore values
  -- that are not following this rule.
  , _percentage :: Maybe Double
  } deriving (Show, Read, Eq)

instance A.ToJSON WorkDoneProgressBeginParams where
    toJSON WorkDoneProgressBeginParams{..} =
        A.object $ catMaybes
            [ Just $ "kind" A..= ("begin" :: Text)
            , Just $ "title" A..= _title
            , ("cancellable" A..=) <$> _cancellable
            , ("message" A..=) <$> _message
            , ("percentage" A..=) <$> _percentage
            ]

instance A.FromJSON WorkDoneProgressBeginParams where
    parseJSON = A.withObject "WorkDoneProgressBegin" $ \o -> do
        kind <- o A..: "kind"
        unless (kind == ("begin" :: Text)) $ fail $ "Expected kind \"begin\" but got " ++ show kind
        _title <- o A..: "title"
        _cancellable <- o A..:? "cancellable"
        _message <- o A..:? "message"
        _percentage <- o A..:? "percentage"
        pure WorkDoneProgressBeginParams{..}

-- The $/progress begin notification is sent from the server to the
-- client to ask the client to start progress.
--
-- @since 0.10.0.0

-- | Parameters for 'WorkDoneProgressReportNotification'
--
-- @since 0.10.0.0
data WorkDoneProgressReportParams =
  WorkDoneProgressReportParams {
    _cancellable :: Maybe Bool
  -- | Optional, more detailed associated progress
  -- message. Contains complementary information to the
  -- '_title'. Examples: "3/25 files",
  -- "project/src/module2", "node_modules/some_dep". If
  -- unset, the previous progress message (if any) is
  -- still valid.
  , _message :: Maybe Text
  -- | Optional progress percentage to display (value 100 is considered 100%).
  -- If infinite progress was indicated in the start notification client
  -- are allowed to ignore the value. In addition the value should be steadily
  -- rising. Clients are free to ignore values that are not following this rule.
  , _percentage :: Maybe Double
  } deriving (Show, Read, Eq)

instance A.ToJSON WorkDoneProgressReportParams where
  toJSON WorkDoneProgressReportParams{..} =
    A.object $ catMaybes
      [ Just $ "kind" A..= ("report" :: Text)
      , ("cancellable" A..=) <$> _cancellable
      , ("message" A..=) <$> _message
      , ("percentage" A..=) <$> _percentage
      ]

instance A.FromJSON WorkDoneProgressReportParams where
  parseJSON = A.withObject "WorkDoneProgressReport" $ \o -> do
    kind <- o A..: "kind"
    unless (kind == ("report" :: Text)) $ fail $ "Expected kind \"report\" but got " ++ show kind
    _cancellable <- o A..:? "cancellable"
    _message <- o A..:? "message"
    _percentage <- o A..:? "percentage"
    pure WorkDoneProgressReportParams{..}

-- The workdone $/progress report notification is sent from the server to the
-- client to report progress for a previously started progress.
--
-- @since 0.10.0.0

-- | Parameters for 'WorkDoneProgressEndNotification'.
--
-- @since 0.10.0.0
data WorkDoneProgressEndParams =
  WorkDoneProgressEndParams {
    _message   :: Maybe Text
  } deriving (Show, Read, Eq)

instance A.ToJSON WorkDoneProgressEndParams where
  toJSON WorkDoneProgressEndParams{..} =
    A.object $ catMaybes
      [ Just $ "kind" A..= ("end" :: Text)
      , ("message" A..=) <$> _message
      ]

instance A.FromJSON WorkDoneProgressEndParams where
  parseJSON = A.withObject "WorkDoneProgressEnd" $ \o -> do
    kind <- o A..: "kind"
    unless (kind == ("end" :: Text)) $ fail $ "Expected kind \"end\" but got " ++ show kind
    _message <- o A..:? "message"
    pure WorkDoneProgressEndParams{..}

--  The $/progress end notification is sent from the server to the
-- client to stop a previously started progress.
--
-- @since 0.10.0.0

-- | Parameters for 'WorkDoneProgressCancelNotification'.
--
-- @since 0.10.0.0
data WorkDoneProgressCancelParams =
  WorkDoneProgressCancelParams {
  -- | A unique identifier to associate multiple progress
  -- notifications with the same progress.
    _token   :: ProgressToken
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''WorkDoneProgressCancelParams

-- The window/workDoneProgress/cancel notification is sent from the client to the server
-- to inform the server that the user has pressed the cancel button on the progress UX.
-- A server receiving a cancel request must still close a progress using the done notification.
--
-- @since 0.10.0.0

data WorkDoneProgressCreateParams =
      WorkDoneProgressCreateParams {
      _token :: ProgressToken
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''WorkDoneProgressCreateParams

data WorkDoneProgressOptions =
  WorkDoneProgressOptions
    { _workDoneProgress :: Maybe Bool
    }
  deriving (Read, Show, Eq)

deriveJSON lspOptions ''WorkDoneProgressOptions

data WorkDoneProgressParams =
  WorkDoneProgressParams
    { -- | An optional token that a server can use to report work done progress
      _workDoneToken :: Maybe ProgressToken
    } deriving (Read,Show,Eq)
deriveJSON lspOptions ''WorkDoneProgressParams

data SomeProgressParams
  = Begin WorkDoneProgressBeginParams
  | Report WorkDoneProgressReportParams
  | End WorkDoneProgressEndParams
  deriving Eq

deriveJSON lspOptionsUntagged ''SomeProgressParams

data PartialResultParams =
  PartialResultParams
    { -- | An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.
      _partialResultToken :: Maybe ProgressToken
    } deriving (Read,Show,Eq)
deriveJSON lspOptions ''PartialResultParams
