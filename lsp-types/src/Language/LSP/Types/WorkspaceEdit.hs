{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.LSP.Types.WorkspaceEdit where

import           Control.Monad                              (unless)
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.HashMap.Strict                        as H
import           Data.Maybe                                 (catMaybes)
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           Data.Hashable

import           Language.LSP.Types.Common
import           Language.LSP.Types.Location
import           Language.LSP.Types.TextDocument
import           Language.LSP.Types.Uri
import           Language.LSP.Types.Utils

-- ---------------------------------------------------------------------

data TextEdit =
  TextEdit
    { _range   :: Range
    , _newText :: Text
    } deriving (Show,Read,Eq)

deriveJSON lspOptions ''TextEdit

-- ---------------------------------------------------------------------

{-|
Additional information that describes document changes.

@since 3.16.0
-}
data ChangeAnnotation =
  ChangeAnnotation
    { -- | A human-readable string describing the actual change. The string
      -- is rendered prominent in the user interface.
      _label             :: Text
      -- | A flag which indicates that user confirmation is needed
      -- before applying the change.
    , _needsConfirmation :: Maybe Bool
      -- | A human-readable string which is rendered less prominent in
      -- the user interface.
    , _description       :: Maybe Text
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ChangeAnnotation

{-|
An identifier referring to a change annotation managed by a workspace
edit.

@since 3.16.0
-}
newtype ChangeAnnotationIdentifier = ChangeAnnotationIdentifierId Text
  deriving (Show, Read, Eq, FromJSON, ToJSON, ToJSONKey, FromJSONKey, Hashable)

makeExtendingDatatype "AnnotatedTextEdit" [''TextEdit]
  [("_annotationId", [t| ChangeAnnotationIdentifier |]) ]
deriveJSON lspOptions ''AnnotatedTextEdit

-- ---------------------------------------------------------------------

data TextDocumentEdit =
  TextDocumentEdit
    { _textDocument :: VersionedTextDocumentIdentifier
    , _edits        :: List (TextEdit |? AnnotatedTextEdit)
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentEdit

-- ---------------------------------------------------------------------

-- | Options to create a file.
data CreateFileOptions =
  CreateFileOptions
    { -- | Overwrite existing file. Overwrite wins over `ignoreIfExists`
      _overwrite      :: Maybe Bool
      -- | Ignore if exists.
    , _ignoreIfExists :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CreateFileOptions

-- | Create file operation
data CreateFile =
  CreateFile
    { -- | The resource to create.
      _uri      :: Uri
      -- | Additional options
    , _options  :: Maybe CreateFileOptions
      -- | An optional annotation identifer describing the operation.
      --
      -- @since 3.16.0
    , _annotationId  :: Maybe ChangeAnnotationIdentifier
    } deriving (Show, Read, Eq)

instance ToJSON CreateFile where
    toJSON CreateFile{..} =
      object $ catMaybes
        [ Just $ "kind" .= ("create" :: Text)
        , Just $ "uri" .= _uri
        , ("options" .=) <$> _options
        , ("annotationId" .=) <$> _annotationId
        ]

instance FromJSON CreateFile where
    parseJSON = withObject "CreateFile" $ \o -> do
        kind <- o .: "kind"
        unless (kind == ("create" :: Text)) 
          $ fail $ "Expected kind \"create\" but got " ++ show kind
        _uri <- o .: "uri"
        _options <- o .:? "options"
        _annotationId <- o .:? "annotationId"
        pure CreateFile{..}

-- Rename file options
data RenameFileOptions =
  RenameFileOptions
    { -- | Overwrite target if existing. Overwrite wins over `ignoreIfExists`
      _overwrite      :: Maybe Bool
      -- | Ignores if target exists.
    , _ignoreIfExists :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''RenameFileOptions

-- | Rename file operation
data RenameFile =
  RenameFile
    { -- | The old (existing) location.
      _oldUri   :: Uri
      -- | The new location.
    , _newUri   :: Uri
      -- | Rename options.
    , _options  :: Maybe RenameFileOptions
      -- | An optional annotation identifer describing the operation.
      --
      -- @since 3.16.0
    , _annotationId  :: Maybe ChangeAnnotationIdentifier
    } deriving (Show, Read, Eq)

instance ToJSON RenameFile where
    toJSON RenameFile{..} =
      object $ catMaybes
        [ Just $ "kind" .= ("rename" :: Text)
        , Just $ "oldUri" .= _oldUri
        , Just $ "newUri" .= _newUri
        , ("options" .=) <$> _options
        , ("annotationId" .=) <$> _annotationId
        ]

instance FromJSON RenameFile where
    parseJSON = withObject "RenameFile" $ \o -> do
        kind <- o .: "kind"
        unless (kind == ("rename" :: Text)) 
          $ fail $ "Expected kind \"rename\" but got " ++ show kind
        _oldUri <- o .: "oldUri"
        _newUri <- o .: "newUri"
        _options <- o .:? "options"
        _annotationId <- o .:? "annotationId"
        pure RenameFile{..}

-- Delete file options
data DeleteFileOptions =
  DeleteFileOptions
    { -- | Delete the content recursively if a folder is denoted.
      _recursive          :: Maybe Bool
      -- | Ignore the operation if the file doesn't exist.
    , _ignoreIfNotExists  :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DeleteFileOptions

-- | Delete file operation
data DeleteFile =
  DeleteFile
    { -- | The file to delete.
      _uri      :: Uri
      -- | Delete options.
    , _options  :: Maybe DeleteFileOptions
      -- | An optional annotation identifer describing the operation.
      --
      -- @since 3.16.0
    , _annotationId  :: Maybe ChangeAnnotationIdentifier
    } deriving (Show, Read, Eq)

instance ToJSON DeleteFile where
    toJSON DeleteFile{..} =
      object $ catMaybes
        [ Just $ "kind" .= ("delete" :: Text)
        , Just $ "uri" .= _uri
        , ("options" .=) <$> _options
        , ("annotationId" .=) <$> _annotationId
        ]

instance FromJSON DeleteFile where
    parseJSON = withObject "DeleteFile" $ \o -> do
        kind <- o .: "kind"
        unless (kind == ("delete" :: Text)) 
          $ fail $ "Expected kind \"delete\" but got " ++ show kind
        _uri <- o .: "uri"
        _options <- o .:? "options"
        _annotationId <- o .:? "annotationId"
        pure DeleteFile{..}

-- ---------------------------------------------------------------------

-- | `TextDocumentEdit |? CreateFile |? RenameFile |? DeleteFile` is a bit mouthful, here's the synonym
type DocumentChange = TextDocumentEdit |? CreateFile |? RenameFile |? DeleteFile

-- ---------------------------------------------------------------------

type WorkspaceEditMap = H.HashMap Uri (List TextEdit)
type ChangeAnnotationMap = H.HashMap ChangeAnnotationIdentifier ChangeAnnotation

data WorkspaceEdit =
  WorkspaceEdit
    {
      -- | Holds changes to existing resources.
      _changes           :: Maybe WorkspaceEditMap
      -- | Depending on the client capability
      -- `workspace.workspaceEdit.resourceOperations` document changes are either
      -- an array of `TextDocumentEdit`s to express changes to n different text
      -- documents where each text document edit addresses a specific version of
      -- a text document. Or it can contain above `TextDocumentEdit`s mixed with
      -- create, rename and delete file / folder operations.
      --
      -- Whether a client supports versioned document edits is expressed via
      -- `workspace.workspaceEdit.documentChanges` client capability.
      --
      -- If a client neither supports `documentChanges` nor
      -- `workspace.workspaceEdit.resourceOperations` then only plain `TextEdit`s
      -- using the `changes` property are supported.
    , _documentChanges   :: Maybe (List DocumentChange)
      -- | A map of change annotations that can be referenced in
      -- `AnnotatedTextEdit`s or create, rename and delete file / folder
      -- operations.
      --
      -- Whether clients honor this property depends on the client capability
      -- `workspace.changeAnnotationSupport`.
      --
      -- @since 3.16.0
    , _changeAnnotations :: Maybe ChangeAnnotationMap
    } deriving (Show, Read, Eq)

instance Semigroup WorkspaceEdit where
  (WorkspaceEdit a b c) <> (WorkspaceEdit a' b' c') = WorkspaceEdit (a <> a') (b <> b') (c <> c')
instance Monoid WorkspaceEdit where
  mempty = WorkspaceEdit Nothing Nothing Nothing

deriveJSON lspOptions ''WorkspaceEdit

-- -------------------------------------

data ResourceOperationKind
  = ResourceOperationCreate -- ^ Supports creating new files and folders.
  | ResourceOperationRename -- ^ Supports renaming existing files and folders.
  | ResourceOperationDelete -- ^ Supports deleting existing files and folders.
  deriving (Read, Show, Eq)
  
instance ToJSON ResourceOperationKind where
  toJSON ResourceOperationCreate = String "create"
  toJSON ResourceOperationRename = String "rename"
  toJSON ResourceOperationDelete = String "delete"

instance FromJSON ResourceOperationKind where
  parseJSON (String "create") = pure ResourceOperationCreate
  parseJSON (String "rename") = pure ResourceOperationRename
  parseJSON (String "delete") = pure ResourceOperationDelete
  parseJSON _                 = fail "ResourceOperationKind"

data FailureHandlingKind
  = FailureHandlingAbort -- ^ Applying the workspace change is simply aborted if one of the changes provided fails. All operations executed before the failing operation stay executed.
  | FailureHandlingTransactional -- ^ All operations are executed transactional. That means they either all succeed or no changes at all are applied to the workspace.
  | FailureHandlingTextOnlyTransactional -- ^ If the workspace edit contains only textual file changes they are executed transactional. If resource changes (create, rename or delete file) are part of the change the failure handling strategy is abort.
  | FailureHandlingUndo -- ^ The client tries to undo the operations already executed. But there is no guarantee that this is succeeding.
  deriving (Read, Show, Eq)
  
instance ToJSON FailureHandlingKind where
  toJSON FailureHandlingAbort                 = String "abort"
  toJSON FailureHandlingTransactional         = String "transactional"
  toJSON FailureHandlingTextOnlyTransactional = String "textOnlyTransactional"
  toJSON FailureHandlingUndo                  = String "undo"

instance FromJSON FailureHandlingKind where
  parseJSON (String "abort")                 = pure FailureHandlingAbort
  parseJSON (String "transactional")         = pure FailureHandlingTransactional
  parseJSON (String "textOnlyTransactional") = pure FailureHandlingTextOnlyTransactional
  parseJSON (String "undo")                  = pure FailureHandlingUndo
  parseJSON _                                = fail "FailureHandlingKind"

data WorkspaceEditChangeAnnotationClientCapabilities =
  WorkspaceEditChangeAnnotationClientCapabilities
  {
    -- | Whether the client groups edits with equal labels into tree nodes,
    -- for instance all edits labelled with "Changes in Strings" would
    -- be a tree node.
    groupsOnLabel :: Maybe Bool
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''WorkspaceEditChangeAnnotationClientCapabilities

data WorkspaceEditClientCapabilities =
  WorkspaceEditClientCapabilities
  { _documentChanges :: Maybe Bool -- ^The client supports versioned document
                                   -- changes in 'WorkspaceEdit's
    -- | The resource operations the client supports. Clients should at least
    -- support @create@, @rename@ and @delete@ files and folders.
  , _resourceOperations :: Maybe (List ResourceOperationKind)
    -- | The failure handling strategy of a client if applying the workspace edit
    -- fails.
  , _failureHandling :: Maybe FailureHandlingKind
    -- | Whether the client normalizes line endings to the client specific
    -- setting.
    --
    -- If set to `true` the client will normalize line ending characters
    -- in a workspace edit to the client specific new line character(s).
    --
    -- @since 3.16.0
  , _normalizesLineEndings :: Maybe Bool
    -- | Whether the client in general supports change annotations on text edits,
    -- create file, rename file and delete file changes.
    --
    -- @since 3.16.0
  , _changeAnnotationSupport :: Maybe WorkspaceEditChangeAnnotationClientCapabilities
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''WorkspaceEditClientCapabilities

-- ---------------------------------------------------------------------

data ApplyWorkspaceEditParams =
  ApplyWorkspaceEditParams
    { -- | An optional label of the workspace edit. This label is
      -- presented in the user interface for example on an undo
      -- stack to undo the workspace edit.
      _label :: Maybe Text
      -- | The edits to apply
    , _edit :: WorkspaceEdit
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ApplyWorkspaceEditParams

data ApplyWorkspaceEditResponseBody =
  ApplyWorkspaceEditResponseBody
    { -- | Indicates whether the edit was applied or not.
      _applied :: Bool
      -- | An optional textual description for why the edit was not applied.
      -- This may be used may be used by the server for diagnostic
      -- logging or to provide a suitable error for a request that
      -- triggered the edit.
    , _failureReason :: Maybe Text
      -- | Depending on the client's failure handling strategy `failedChange`
      -- might contain the index of the change that failed. This property is
      -- only available if the client signals a `failureHandling` strategy
      -- in its client capabilities.
    , _failedChange :: Maybe UInt
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ApplyWorkspaceEditResponseBody

-- ---------------------------------------------------------------------

-- | Applies a 'TextEdit' to some 'Text'.
-- >>> applyTextEdit (TextEdit (Range (Position 0 1) (Position 0 2)) "i") "foo"
-- "fio"
applyTextEdit :: TextEdit -> Text -> Text
applyTextEdit (TextEdit (Range sp ep) newText) oldText =
  let (_, afterEnd) = splitAtPos ep oldText
      (beforeStart, _) = splitAtPos sp oldText
    in mconcat [beforeStart, newText, afterEnd]
  where
    splitAtPos :: Position -> Text -> (Text, Text)
    splitAtPos (Position sl sc) t =
      -- If we are looking for a line beyond the end of the text, this will give us an index
      -- past the end. Fortunately, T.splitAt is fine with this, and just gives us the whole
      -- string and an empty string, which is what we want.
      let index = sc + startLineIndex sl t
        in T.splitAt (fromIntegral index) t

    -- The index of the first character of line 'line'
    startLineIndex :: UInt -> Text -> UInt
    startLineIndex 0 _ = 0
    startLineIndex line t' =
      case T.findIndex (== '\n') t' of
        Just i -> fromIntegral i + 1 + startLineIndex (line - 1) (T.drop (i + 1) t')
        -- i != 0, and there are no newlines, so this is a line beyond the end of the text.
        -- In this case give the "start index" as the end, so we will at least append the text.
        Nothing -> fromIntegral $ T.length t'

-- | 'editTextEdit' @outer@ @inner@ applies @inner@ to the text inside @outer@.
editTextEdit :: TextEdit -> TextEdit -> TextEdit
editTextEdit (TextEdit origRange origText) innerEdit =
  let newText = applyTextEdit innerEdit origText
    in TextEdit origRange newText
