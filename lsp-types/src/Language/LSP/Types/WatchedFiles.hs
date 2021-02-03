{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.LSP.Types.WatchedFiles where
  
import Data.Aeson
import Data.Aeson.TH
import Data.Bits
import Data.Scientific
import Language.LSP.Types.Common
import Language.LSP.Types.Uri
import Language.LSP.Types.Utils
import Data.Text (Text)

-- -------------------------------------

data DidChangeWatchedFilesClientCapabilities = DidChangeWatchedFilesClientCapabilities
  { -- | Did change watched files notification supports dynamic
    -- registration.
    _dynamicRegistration :: Maybe Bool
  }
  deriving (Show, Read, Eq)
deriveJSON lspOptions ''DidChangeWatchedFilesClientCapabilities

-- | Describe options to be used when registering for file system change events.
data DidChangeWatchedFilesRegistrationOptions =
  DidChangeWatchedFilesRegistrationOptions
  { -- | The watchers to register.
    _watchers :: List FileSystemWatcher
  } deriving (Show, Read, Eq)

data FileSystemWatcher =
  FileSystemWatcher
  { -- | The glob pattern to watch.
    -- Glob patterns can have the following syntax:
    -- - @*@ to match one or more characters in a path segment
    -- - @?@ to match on one character in a path segment
    -- - @**@ to match any number of path segments, including none
    -- - @{}@ to group conditions (e.g. @**​/*.{ts,js}@ matches all TypeScript and JavaScript files)
    -- - @[]@ to declare a range of characters to match in a path segment (e.g., @example.[0-9]@ to match on @example.0@, @example.1@, …)
    -- - @[!...]@ to negate a range of characters to match in a path segment (e.g., @example.[!0-9]@ to match on @example.a@, @example.b@, but not @example.0@)
    _globPattern :: Text,
    -- | The kind of events of interest. If omitted it defaults
    -- to WatchKind.Create | WatchKind.Change | WatchKind.Delete
    -- which is 7.
    _kind :: Maybe WatchKind
  } deriving (Show, Read, Eq)

data WatchKind =
  WatchKind {
    -- | Watch for create events
    _watchCreate :: Bool,
    -- | Watch for change events
    _watchChange :: Bool,
    -- | Watch for delete events
    _watchDelete :: Bool
  } deriving (Show, Read, Eq)

instance ToJSON WatchKind where
  toJSON wk = Number (createNum + changeNum + deleteNum)
    where
      createNum = if _watchCreate wk then 0x1 else 0x0
      changeNum = if _watchChange wk then 0x2 else 0x0
      deleteNum = if _watchDelete wk then 0x4 else 0x0

instance FromJSON WatchKind where
  parseJSON (Number n)
    | Right i <- floatingOrInteger n :: Either Double Int
    , 0 <= i && i <= 7 =
        pure $ WatchKind (testBit i 0x0) (testBit i 0x1) (testBit i 0x2)
    | otherwise = mempty
  parseJSON _            = mempty

deriveJSON lspOptions ''DidChangeWatchedFilesRegistrationOptions
deriveJSON lspOptions ''FileSystemWatcher

-- | The file event type.
data FileChangeType = FcCreated -- ^ The file got created.
                    | FcChanged -- ^ The file got changed.
                    | FcDeleted -- ^ The file got deleted.
       deriving (Read,Show,Eq)

instance ToJSON FileChangeType where
  toJSON FcCreated = Number 1
  toJSON FcChanged = Number 2
  toJSON FcDeleted = Number 3

instance FromJSON FileChangeType where
  parseJSON (Number 1) = pure FcCreated
  parseJSON (Number 2) = pure FcChanged
  parseJSON (Number 3) = pure FcDeleted
  parseJSON _            = mempty


-- -------------------------------------

-- | An event describing a file change.
data FileEvent =
  FileEvent
  { -- | The file's URI.
    _uri   :: Uri
    -- | The change type.
  , _xtype :: FileChangeType
  } deriving (Read,Show,Eq)

deriveJSON lspOptions ''FileEvent

data DidChangeWatchedFilesParams =
  DidChangeWatchedFilesParams
  { -- | The actual file events.
    _changes :: List FileEvent
  } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DidChangeWatchedFilesParams
