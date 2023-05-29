-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceEdit where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Map
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.ChangeAnnotation
import qualified Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier
import qualified Language.LSP.Protocol.Internal.Types.CreateFile
import qualified Language.LSP.Protocol.Internal.Types.DeleteFile
import qualified Language.LSP.Protocol.Internal.Types.RenameFile
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentEdit
import qualified Language.LSP.Protocol.Internal.Types.TextEdit
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
A workspace edit represents changes to many resources managed in the workspace. The edit
should either provide `changes` or `documentChanges`. If documentChanges are present
they are preferred over `changes` if the client can handle versioned document edits.

Since version 3.13.0 a workspace edit can contain resource operations as well. If resource
operations are present clients need to execute the operations in the order in which they
are provided. So a workspace edit for example can consist of the following two changes:
(1) a create file a.txt and (2) a text document edit which insert text into file a.txt.

An invalid sequence (e.g. (1) delete file a.txt and (2) insert text into file a.txt) will
cause failure of the operation. How the client recovers from the failure is described by
the client capability: `workspace.workspaceEdit.failureHandling`
-}
data WorkspaceEdit = WorkspaceEdit 
  { {-|
  Holds changes to existing resources.
  -}
  _changes :: (Maybe (Data.Map.Map Language.LSP.Protocol.Types.Uri.Uri [Language.LSP.Protocol.Internal.Types.TextEdit.TextEdit]))
  , {-|
  Depending on the client capability `workspace.workspaceEdit.resourceOperations` document changes
  are either an array of `TextDocumentEdit`s to express changes to n different text documents
  where each text document edit addresses a specific version of a text document. Or it can contain
  above `TextDocumentEdit`s mixed with create, rename and delete file / folder operations.

  Whether a client supports versioned document edits is expressed via
  `workspace.workspaceEdit.documentChanges` client capability.

  If a client neither supports `documentChanges` nor `workspace.workspaceEdit.resourceOperations` then
  only plain `TextEdit`s using the `changes` property are supported.
  -}
  _documentChanges :: (Maybe [(Language.LSP.Protocol.Internal.Types.TextDocumentEdit.TextDocumentEdit Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.CreateFile.CreateFile Language.LSP.Protocol.Types.Common.|? (Language.LSP.Protocol.Internal.Types.RenameFile.RenameFile Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.DeleteFile.DeleteFile)))])
  , {-|
  A map of change annotations that can be referenced in `AnnotatedTextEdit`s or create, rename and
  delete file / folder operations.

  Whether clients honor this property depends on the client capability `workspace.changeAnnotationSupport`.

  @since 3.16.0
  -}
  _changeAnnotations :: (Maybe (Data.Map.Map Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier.ChangeAnnotationIdentifier Language.LSP.Protocol.Internal.Types.ChangeAnnotation.ChangeAnnotation))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON WorkspaceEdit where
  toJSON (WorkspaceEdit arg0 arg1 arg2) = Aeson.object $ concat $  ["changes" Language.LSP.Protocol.Types.Common..=? arg0
    ,"documentChanges" Language.LSP.Protocol.Types.Common..=? arg1
    ,"changeAnnotations" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON WorkspaceEdit where
  parseJSON = Aeson.withObject "WorkspaceEdit" $ \arg -> WorkspaceEdit <$> arg Aeson..:! "changes" <*> arg Aeson..:! "documentChanges" <*> arg Aeson..:! "changeAnnotations"