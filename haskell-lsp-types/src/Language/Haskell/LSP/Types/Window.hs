{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.Haskell.LSP.Types.Window where

import qualified Data.Aeson                                 as A
import           Data.Aeson.TH
import           Data.Text                                  (Text)
import           Language.Haskell.LSP.Types.Constants
import           Language.Haskell.LSP.Types.Message

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
-- ---------------------------------------------------------------------
{-
Progress Start Notification

The window/progress/start notification is sent from the server to the client to ask the client to start progress.

Notification:

method: 'window/progress/start'
params: ProgressStartParams defined as follows:
export interface ProgressStartParams {

  /**
   * A unique identifier to associate multiple progress notifications with
   * the same progress.
   */
  id: string;

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
   * complementary information to the '_title'.
   *
   * Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
   * If unset, the previous progress message (if any) is still valid.
   */
  message?: string;

  /**
   * Optional progress percentage to display (value 100 is considered 100%).
   * If not provided infinite progress is assumed and clients are allowed
   * to ignore the '_percentage' value in subsequent in report notifications.
   *
   * The value should be steadily rising. Clients are free to ignore values
   * that are not following this rule.
   */
  percentage?: number;
}
-}

-- | Parameters for 'ProgressStartNotification'.
--
-- @since 0.10.0.0
data ProgressStartParams =
  ProgressStartParams {
  -- | A unique identifier to associate multiple progress
  -- notifications with the same progress.
    _id   :: Text
  -- | Mandatory title of the progress operation.
  -- Used to briefly inform about the kind of operation being
  -- performed. Examples: "Indexing" or "Linking dependencies".
  , _title :: Text
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

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''ProgressStartParams

{-
Progress Report Notification

The window/progress/report notification is sent from the server to the client to report progress for a previously started progress.

Notification:

method: 'window/progress/report'
params: ProgressReportParams defined as follows:
export interface ProgressReportParams {

  /**
   * A unique identifier to associate multiple progress notifications with the same progress.
   */
  id: string;

  /**
   * Optional, more detailed associated progress message. Contains
   * complementary information to the '_title'.
   *
   * Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
   * If unset, the previous progress message (if any) is still valid.
   */
  message?: string;

  /**
   * Optional progress percentage to display (value 100 is considered 100%).
   * If infinite progress was indicated in the start notification client
   * are allowed to ignore the value. In addition the value should be steadily
   * rising. Clients are free to ignore values that are not following this rule.
   */
  percentage?: number;
}

-}

-- | Parameters for 'ProgressReportNotification'
--
-- @since 0.10.0.0
data ProgressReportParams =
  ProgressReportParams {
  -- | A unique identifier to associate multiple progress
  -- notifications with the same progress.
    _id   :: Text
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

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''ProgressReportParams

{-
Progress Done Notification

The window/progress/done notification is sent from the server to the client to stop a previously started progress.

Notification:

method: 'window/progress/done'
params: ProgressDoneParams defined as follows:
export interface ProgressDoneParams {
  /**
   * A unique identifier to associate multiple progress notifications with the same progress.
   */
  id: string;
}
-}

-- | Parameters for 'ProgressDoneNotification'.
--
-- @since 0.10.0.0
data ProgressDoneParams =
  ProgressDoneParams {
  -- | A unique identifier to associate multiple progress
  -- notifications with the same progress.
    _id   :: Text
  } deriving (Show, Read, Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''ProgressDoneParams

{-
Progress Cancel Notification

The window/progress/cancel notification is sent from the client to the server to inform the server that the user has pressed the cancel button on the progress UX. A server receiving a cancel request must still close a progress using the done notification.

Notification:

method: 'window/progress/cancel'
params: ProgressCancelParams defined as follows:
export interface ProgressCancelParams {
  /**
   * A unique identifier to associate multiple progress notifications with the same progress.
   */
  id: string;
}

-}

-- | Parameters for 'ProgressCancelNotification'.
--
-- @since 0.10.0.0
data ProgressCancelParams =
  ProgressCancelParams {
  -- | A unique identifier to associate multiple progress
  -- notifications with the same progress.
    _id   :: Text
  } deriving (Show, Read, Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''ProgressCancelParams

