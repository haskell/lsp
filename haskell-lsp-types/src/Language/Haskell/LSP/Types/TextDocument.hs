{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
module Language.Haskell.LSP.Types.TextDocument where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Default
import           Data.Text                      ( Text )

import           Language.Haskell.LSP.Types.Common
import           Language.Haskell.LSP.Types.DocumentFilter
import           Language.Haskell.LSP.Types.Location
import           Language.Haskell.LSP.Types.Uri
import           Language.Haskell.LSP.Types.Utils

-- ---------------------------------------------------------------------

data TextDocumentIdentifier =
  TextDocumentIdentifier
    { _uri :: Uri
    } deriving (Show, Read, Eq)
deriveJSON lspOptions ''TextDocumentIdentifier

type TextDocumentVersion = Maybe Int

makeExtendingDatatype "VersionedTextDocumentIdentifier" [''TextDocumentIdentifier]
  [ ("_version", [t| TextDocumentVersion |])]
deriveJSON lspOptions ''VersionedTextDocumentIdentifier

data TextDocumentItem =
  TextDocumentItem {
    _uri        :: Uri
  , _languageId :: Text
  , _version    :: Int
  , _text       :: Text
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentItem

-- ---------------------------------------------------------------------

data TextDocumentPositionParams =
  TextDocumentPositionParams
    { -- | The text document.
      _textDocument :: TextDocumentIdentifier
    , -- | The position inside the text document.
      _position     :: Position
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentPositionParams

-- -------------------------------------

-- Text document synchronisation


data TextDocumentSyncClientCapabilities =
  TextDocumentSyncClientCapabilities
    { -- | Whether text document synchronization supports dynamic registration.
      _dynamicRegistration :: Maybe Bool

      -- | The client supports sending will save notifications.
    , _willSave :: Maybe Bool

      -- | The client supports sending a will save request and waits for a
      -- response providing text edits which will be applied to the document
      -- before it is saved.
    , _willSaveWaitUntil :: Maybe Bool

      -- | The client supports did save notifications.
    , _didSave :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentSyncClientCapabilities

instance Default TextDocumentSyncClientCapabilities where
  def = TextDocumentSyncClientCapabilities def def def def

-- -------------------------------------

data SaveOptions =
  SaveOptions
  { -- | The client is supposed to include the content on save.
    _includeText :: Maybe Bool
  } deriving (Show, Read, Eq)

-- -------------------------------------

-- | Defines how the host (editor) should sync document changes to the language server.
data TextDocumentSyncKind
  = -- | Documents should not be synced at all.
    TdSyncNone
  | -- | Documents are synced by always sending the full content of the document.
    TdSyncFull
  | -- | Documents are synced by sending the full content on open. After that only incremental updates to the document are send.
    TdSyncIncremental
  deriving (Read, Eq, Show)

instance ToJSON TextDocumentSyncKind where
  toJSON TdSyncNone        = Number 0
  toJSON TdSyncFull        = Number 1
  toJSON TdSyncIncremental = Number 2

instance FromJSON TextDocumentSyncKind where
  parseJSON (Number 0) = pure TdSyncNone
  parseJSON (Number 1) = pure TdSyncFull
  parseJSON (Number 2) = pure TdSyncIncremental
  parseJSON _            = mempty
  
data TextDocumentSyncOptions =
  TextDocumentSyncOptions
  { -- | Open and close notifications are sent to the server. If omitted open
    -- close notification should not be sent.
    _openClose :: Maybe Bool
  , -- | Change notifications are sent to the server. See
    -- TextDocumentSyncKind.None, TextDocumentSyncKind.Full
    -- and TextDocumentSyncKind.Incremental. If omitted it defaults to
    -- TextDocumentSyncKind.None.
    _change    :: Maybe TextDocumentSyncKind
    -- | If present will save notifications are sent to the server. If omitted the notification should not be
    -- sent.
  , _willSave  :: Maybe Bool
    -- | If present will save wait until requests are sent to the server. If omitted the request should not be
    -- sent.
  , _willSaveWaitUntil :: Maybe Bool
    -- | If present save notifications are sent to the server. If omitted the
    -- notification should not be sent.
  , _save      :: Maybe (Bool |? SaveOptions)
  } deriving (Show, Read, Eq)
deriveJSON lspOptions ''TextDocumentSyncOptions

-- -------------------------------------

{-
Since most of the registration options require to specify a document selector
there is a base interface that can be used.
-}

data TextDocumentRegistrationOptions =
  TextDocumentRegistrationOptions
    { _documentSelector :: Maybe DocumentSelector
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentRegistrationOptions

-- -------------------------------------

data DidOpenTextDocumentParams =
  DidOpenTextDocumentParams
  { -- | The document that was opened.
    _textDocument :: TextDocumentItem
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DidOpenTextDocumentParams

-- -------------------------------------

makeExtendingDatatype "TextDocumentChangeRegistrationOptions"
  [''TextDocumentRegistrationOptions]
  [("_syncKind", [t| TextDocumentSyncKind |])]

deriveJSON lspOptions ''TextDocumentChangeRegistrationOptions

{-# DEPRECATED _rangeLength "Use _range instead" #-}
data TextDocumentContentChangeEvent =
  TextDocumentContentChangeEvent
    { -- | The range of the document that changed.
      _range       :: Maybe Range
      -- | The optional length of the range that got replaced.
    , _rangeLength :: Maybe Int
      -- | The new text for the provided range, if provided.
      -- Otherwise the new text of the whole document.
    , _text        :: Text
    }
  deriving (Read,Show,Eq)

deriveJSON lspOptions ''TextDocumentContentChangeEvent

-- -------------------------------------

data DidChangeTextDocumentParams =
  DidChangeTextDocumentParams
    { -- | The document that did change. The version number points
      -- to the version after all provided content changes have
      -- been applied.
      _textDocument   :: VersionedTextDocumentIdentifier
      -- | The actual content changes. The content changes describe single state changes
      -- to the document. So if there are two content changes c1 (at array index 0) and
      -- c2 (at array index 1) for a document in state S then c1 moves the document from
      -- S to S' and c2 from S' to S''. So c1 is computed on the state S and c2 is computed
      -- on the state S'.
      --
      -- To mirror the content of a document using change events use the following approach:
      -- - start with the same initial content
      -- - apply the 'textDocument/didChange' notifications in the order you recevie them.
      -- - apply the `TextDocumentContentChangeEvent`s in a single notification in the order
      --   you receive them.
    , _contentChanges :: List TextDocumentContentChangeEvent
    } deriving (Show,Read,Eq)

deriveJSON lspOptions ''DidChangeTextDocumentParams

-- -------------------------------------

data TextDocumentSaveReason
  = SaveManual
         -- ^ Manually triggered, e.g. by the user pressing save, by starting
         -- debugging, or by an API call.
  | SaveAfterDelay -- ^ Automatic after a delay
  | SaveFocusOut   -- ^ When the editor lost focus
  deriving (Show, Read, Eq)

instance ToJSON TextDocumentSaveReason where
  toJSON SaveManual     = Number 1
  toJSON SaveAfterDelay = Number 2
  toJSON SaveFocusOut   = Number 3

instance FromJSON TextDocumentSaveReason where
  parseJSON (Number 1) = pure SaveManual
  parseJSON (Number 2) = pure SaveAfterDelay
  parseJSON (Number 3) = pure SaveFocusOut
  parseJSON _          = mempty

data WillSaveTextDocumentParams =
  WillSaveTextDocumentParams
    { -- | The document that will be saved.
      _textDocument :: TextDocumentIdentifier
      -- | The 'TextDocumentSaveReason'.
    , _reason       :: TextDocumentSaveReason
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''WillSaveTextDocumentParams

-- -------------------------------------

deriveJSON lspOptions ''SaveOptions

makeExtendingDatatype "TextDocumentSaveRegistrationOptions"
  [''TextDocumentRegistrationOptions]
  [("_includeText", [t| Maybe Bool |])]

deriveJSON lspOptions ''TextDocumentSaveRegistrationOptions

data DidSaveTextDocumentParams =
  DidSaveTextDocumentParams
    { -- | The document that was saved.
      _textDocument :: TextDocumentIdentifier
      -- | Optional the content when saved. Depends on the includeText value
      -- when the save notification was requested.
    , _text         :: Maybe Text
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DidSaveTextDocumentParams

-- -------------------------------------

data DidCloseTextDocumentParams =
  DidCloseTextDocumentParams
    { -- | The document that was closed.
      _textDocument :: TextDocumentIdentifier
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DidCloseTextDocumentParams
