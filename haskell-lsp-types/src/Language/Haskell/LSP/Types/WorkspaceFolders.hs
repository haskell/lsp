{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Haskell.LSP.Types.WorkspaceFolders where

import           Data.Aeson.TH
import           Data.Text                      ( Text )

import           Language.Haskell.LSP.Types.Common
import           Language.Haskell.LSP.Types.Utils

{-
Workspace folders request (:arrow_right_hook:)
Since version 3.6.0

Many tools support more than one root folder per workspace. Examples for this
are VS Code’s multi-root support, Atom’s project folder support or Sublime’s
project support. If a client workspace consists of multiple roots then a server
typically needs to know about this. The protocol up to now assumes one root
folder which is announce to the server by the rootUri property of the
InitializeParams. If the client supports workspace folders and announces them
via the corrsponding workspaceFolders client capability the InitializeParams
contain an additional property workspaceFolders with the configured workspace
folders when the server starts.

The workspace/workspaceFolders request is sent from the server to the client to
fetch the current open list of workspace folders. Returns null in the response
if only a single file is open in the tool. Returns an empty array if a workspace
is open but no folders are configured.

Request:

method: ‘workspace/workspaceFolders’
params: none
Response:

result: WorkspaceFolder[] | null defines as follows:
export interface WorkspaceFolder {
	/**
	 * The associated URI for this workspace folder.
	 */
	uri: string;

	/**
	 * The name of the workspace folder. Defaults to the
	 * uri's basename.
	 */
	name: string;
}
error: code and message set in case an exception happens during the ‘workspace/workspaceFolders’ request
-}

data WorkspaceFolder =
  WorkspaceFolder
    { -- | The name of the workspace folder. Defaults to the uri's basename.
      _uri  :: Text
    -- | The name of the workspace folder. Defaults to the uri's basename.
    , _name :: Text
    } deriving (Read, Show, Eq)

deriveJSON lspOptions ''WorkspaceFolder
{-
DidChangeWorkspaceFolders Notification (:arrow_right:)
Since version 3.6.0

The workspace/didChangeWorkspaceFolders notification is sent from the client to
the server to inform the server about workspace folder configuration changes.
The notification is sent by default if both
ServerCapabilities/workspace/workspaceFolders and
ClientCapabilities/workspace/workspaceFolders are true; or if the server has
registered to receive this notification it first. To register for the
workspace/didChangeWorkspaceFolders send a client/registerCapability request
from the client to the server. The registration parameter must have a
registrations item of the following form, where id is a unique id used to
unregister the capability (the example uses a UUID):

{
	id: "28c6150c-bd7b-11e7-abc4-cec278b6b50a",
	method: "workspace/didChangeWorkspaceFolders"
}
Notification:

method: ‘workspace/didChangeWorkspaceFolders’
params: DidChangeWorkspaceFoldersParams defined as follows:
export interface DidChangeWorkspaceFoldersParams {
	/**
	 * The actual workspace folder change event.
	 */
	event: WorkspaceFoldersChangeEvent;
}

/**
 * The workspace folder change event.
 */
export interface WorkspaceFoldersChangeEvent {
	/**
	 * The array of added workspace folders
	 */
	added: WorkspaceFolder[];

	/**
	 * The array of the removed workspace folders
	 */
	removed: WorkspaceFolder[];
}
-}

-- | The workspace folder change event.
data WorkspaceFoldersChangeEvent =
  WorkspaceFoldersChangeEvent
    { _added :: List WorkspaceFolder -- ^ The array of added workspace folders
    , _removed :: List WorkspaceFolder -- ^ The array of the removed workspace folders
    } deriving (Read, Show, Eq)

deriveJSON lspOptions ''WorkspaceFoldersChangeEvent

data DidChangeWorkspaceFoldersParams = 
  DidChangeWorkspaceFoldersParams
    { _event :: WorkspaceFoldersChangeEvent
      -- ^ The actual workspace folder change event.
    } deriving (Read, Show, Eq)

deriveJSON lspOptions ''DidChangeWorkspaceFoldersParams

