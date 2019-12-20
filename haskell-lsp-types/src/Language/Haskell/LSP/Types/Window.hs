{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveFunctor              #-}
module Language.Haskell.LSP.Types.Window where

import           Control.Applicative
import           Control.Monad (unless)
import qualified Data.Aeson                                 as A
import           Data.Aeson.TH
import           Data.Maybe (catMaybes)
import           Data.Text                                  (Text)
import           Language.Haskell.LSP.Types.Constants
import           Language.Haskell.LSP.Types.Progress

-- ---------------------------------------------------------------------
{-
ShowMessage Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#showmessage-notification

The show message notification is sent from a server to a client to ask the
client to display a particular message in the user interface.

Notification:

    method: 'window/showMessage'
    params: ShowMessageParams defined as follows:

interface ShowMessageParams {
    /**
     * The message type. See {@link MessageType}.
     */
    type: number;

    /**
     * The actual message.
     */
    message: string;
}

Where the type is defined as follows:

enum MessageType {
    /**
     * An error message.
     */
    Error = 1,
    /**
     * A warning message.
     */
    Warning = 2,
    /**
     * An information message.
     */
    Info = 3,
    /**
     * A log message.
     */
    Log = 4
}
-}
data MessageType = MtError   -- ^ Error = 1,
                 | MtWarning -- ^ Warning = 2,
                 | MtInfo    -- ^ Info = 3,
                 | MtLog     -- ^ Log = 4
        deriving (Eq,Ord,Show,Read,Enum)

instance A.ToJSON MessageType where
  toJSON MtError   = A.Number 1
  toJSON MtWarning = A.Number 2
  toJSON MtInfo    = A.Number 3
  toJSON MtLog     = A.Number 4

instance A.FromJSON MessageType where
  parseJSON (A.Number 1) = pure MtError
  parseJSON (A.Number 2) = pure MtWarning
  parseJSON (A.Number 3) = pure MtInfo
  parseJSON (A.Number 4) = pure MtLog
  parseJSON _            = mempty

-- ---------------------------------------


data ShowMessageParams =
  ShowMessageParams {
    _xtype   :: MessageType
  , _message :: Text
  } deriving (Show, Read, Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''ShowMessageParams

-- ---------------------------------------------------------------------
{-
ShowMessage Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#showmessage-request

    New: The show message request is sent from a server to a client to ask the
    client to display a particular message in the user interface. In addition to
    the show message notification the request allows to pass actions and to wait
    for an answer from the client.

Request:

    method: 'window/showMessageRequest'
    params: ShowMessageRequestParams defined as follows:

Response:

    result: the selected MessageActionItem
    error: code and message set in case an exception happens during showing a message.

interface ShowMessageRequestParams {
    /**
     * The message type. See {@link MessageType}
     */
    type: number;

    /**
     * The actual message
     */
    message: string;

    /**
     * The message action items to present.
     */
    actions?: MessageActionItem[];
}

Where the MessageActionItem is defined as follows:

interface MessageActionItem {
    /**
     * A short title like 'Retry', 'Open Log' etc.
     */
    title: string;
}
-}

data MessageActionItem =
  MessageActionItem
    { _title :: Text
    } deriving (Show,Read,Eq)

deriveJSON lspOptions ''MessageActionItem


data ShowMessageRequestParams =
  ShowMessageRequestParams
    { _xtype   :: MessageType
    , _message :: Text
    , _actions :: Maybe [MessageActionItem]
    } deriving (Show,Read,Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''ShowMessageRequestParams

-- ---------------------------------------------------------------------
{-
LogMessage Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#logmessage-notification

The log message notification is sent from the server to the client to ask the
client to log a particular message.

Notification:

    method: 'window/logMessage'
    params: LogMessageParams defined as follows:

interface LogMessageParams {
    /**
     * The message type. See {@link MessageType}
     */
    type: number;

    /**
     * The actual message
     */
    message: string;
}

Where type is defined as above.
-}

data LogMessageParams =
  LogMessageParams {
    _xtype   :: MessageType
  , _message :: Text
  } deriving (Show, Read, Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''LogMessageParams

{-
Progress Begin Notification

To start progress reporting a $/progress notification with the following payload must be sent:

export interface WorkDoneProgressBegin {

	kind: 'begin';

	/**
	 * Mandatory title of the progress operation. Used to briefly inform about
	 * the kind of operation being performed.
	 *
	 * Examples: "Indexing" or "Linking dependencies".
	 */
	title: string;

	/**
	 * Controls if a cancel button should show to allow the user to cancel the
	 * long running operation. Clients that don't support cancellation are allowed
	 * to ignore the setting.
	 */
	cancellable?: boolean;

	/**
	 * Optional, more detailed associated progress message. Contains
	 * complementary information to the `title`.
	 *
	 * Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
	 * If unset, the previous progress message (if any) is still valid.
	 */
	message?: string;

	/**
	 * Optional progress percentage to display (value 100 is considered 100%).
	 * If not provided infinite progress is assumed and clients are allowed
	 * to ignore the `percentage` value in subsequent in report notifications.
	 *
	 * The value should be steadily rising. Clients are free to ignore values
	 * that are not following this rule.
	 */
	percentage?: number;
-}

-- | Parameters for a $/progress notification.
data ProgressParams t =
    ProgressParams {
      _token :: ProgressToken
    , _value :: t
    } deriving (Show, Read, Eq, Functor)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''ProgressParams

data SomeProgressParams
  = Begin WorkDoneProgressBeginParams
  | Report WorkDoneProgressReportParams
  | End WorkDoneProgressEndParams
  deriving Eq

instance A.FromJSON SomeProgressParams where
  parseJSON x =
       (Begin  <$> A.parseJSON x)
   <|> (Report <$> A.parseJSON x)
   <|> (End    <$> A.parseJSON x)

instance A.ToJSON SomeProgressParams where
  toJSON (Begin  x) = A.toJSON x
  toJSON (Report x) = A.toJSON x
  toJSON (End    x) = A.toJSON x

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

-- | The $/progress begin notification is sent from the server to the
-- client to ask the client to start progress.
--
-- @since 0.10.0.0
{-
Progress Report Notification

Reporting progress is done using the following payload:

export interface WorkDoneProgressReport {

	kind: 'report';

	/**
	 * Controls enablement state of a cancel button. This property is only valid if a cancel
	 * button got requested in the `WorkDoneProgressStart` payload.
	 *
	 * Clients that don't support cancellation or don't support control the button's
	 * enablement state are allowed to ignore the setting.
	 */
	cancellable?: boolean;

	/**
	 * Optional, more detailed associated progress message. Contains
	 * complementary information to the `title`.
	 *
	 * Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
	 * If unset, the previous progress message (if any) is still valid.
	 */
	message?: string;

	/**
	 * Optional progress percentage to display (value 100 is considered 100%).
	 * If not provided infinite progress is assumed and clients are allowed
	 * to ignore the `percentage` value in subsequent in report notifications.
	 *
	 * The value should be steadily rising. Clients are free to ignore values
	 * that are not following this rule.
	 */
	percentage?: number;
}
-}

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

-- | The workdone $/progress report notification is sent from the server to the
-- client to report progress for a previously started progress.
--
-- @since 0.10.0.0
{-
Progress End Notification

Signaling the end of a progress reporting is done using the following payload:

export interface WorkDoneProgressEnd {

	kind: 'end';

	/**
	 * Optional, a final message indicating to for example indicate the outcome
	 * of the operation.
	 */
	message?: string;
}
-}

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

-- | The $/progress end notification is sent from the server to the
-- client to stop a previously started progress.
--
-- @since 0.10.0.0
{-
Progress Cancel Notification

The window/workDoneProgress/cancel notification is sent from the client to the server to inform the server that the user has pressed the cancel button on the progress UX. A server receiving a cancel request must still close a progress using the done notification.

Notification:

method: 'window/workDoneProgress/cancel'
params: WorkDoneProgressCancelParams defined as follows:
export interface WorkDoneProgressCancelParams {
	/**
	 * The token to be used to report progress.
	 */
	token: ProgressToken;
}
-}

-- | Parameters for 'WorkDoneProgressCancelNotification'.
--
-- @since 0.10.0.0
data WorkDoneProgressCancelParams =
  WorkDoneProgressCancelParams {
  -- | A unique identifier to associate multiple progress
  -- notifications with the same progress.
    _token   :: ProgressToken
  } deriving (Show, Read, Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''WorkDoneProgressCancelParams

-- | The window/workDoneProgress/cancel notification is sent from the client to the server
-- to inform the server that the user has pressed the cancel button on the progress UX.
-- A server receiving a cancel request must still close a progress using the done notification.
--
-- @since 0.10.0.0

data WorkDoneProgressCreateParams =
      WorkDoneProgressCreateParams {
      _token :: ProgressToken
    } deriving (Show, Read, Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''WorkDoneProgressCreateParams

