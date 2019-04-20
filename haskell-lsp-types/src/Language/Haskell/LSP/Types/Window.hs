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

type ShowMessageNotification = NotificationMessage ServerMethod ShowMessageParams

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

type ShowMessageRequest = RequestMessage ServerMethod ShowMessageRequestParams Text
type ShowMessageResponse = ResponseMessage Text

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


type LogMessageNotification = NotificationMessage ServerMethod LogMessageParams

-- ---------------------------------------------------------------------
{-
Progress Notification

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#logmessage-notification

The progress notification is sent from the server to the client to ask the
client to indicate progress.

Notification:

    method: 'window/progress'
    params: ProgressParams defined as follows:

interface ProgressParams {
  /**
   * A unique identifier to associate multiple progress notifications with the same progress.
   */
  id: string;

  /**
   * The title of the progress.
   * This should be the same for all ProgressParams with the same id.
   */
  title: string;

  /**
   * Optional progress message to display.
   * If unset, the previous progress message (if any) is still valid.
   */
  message?: string;

  /**
   * Optional progress percentage to display.
   * If unset, the previous progress percentage (if any) is still valid.
   */
  percentage?: number;

  /**
   * Set to true on the final progress update.
   * No more progress notifications with the same ID should be sent.
   */
  done?: boolean;
}

-}

data ProgressParams =
  ProgressParams {
    _id   :: Text -- ^ A unique identifier to associate multiple progress
                  -- notifications with the same progress.
  , _title :: Text -- ^ Mandatory title of the progress operation.
                   -- Used to briefly inform about the kind of operation being
                   -- performed. Examples: "Indexing" or "Linking dependencies".
  , _message :: Maybe Text -- ^ Optional, more detailed associated progress
                           -- message. Contains complementary information to the
                           -- `title`. Examples: "3/25 files",
                           -- "project/src/module2", "node_modules/some_dep". If
                           -- unset, the previous progress message (if any) is
                           -- still valid.
  , _percentage :: Maybe Double -- ^ Optional progress percentage to display
                                -- (value 100 is considered 100%). If unset, the
                                -- previous progress percentage (if any) is
                                -- still valid.
  , _done :: Maybe Bool -- ^ Set to true on the final progress update. No more
                        -- progress notifications with the same ID should be sent.
  } deriving (Show, Read, Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''ProgressParams

type ProgressNotification = NotificationMessage ServerMethod ProgressParams

