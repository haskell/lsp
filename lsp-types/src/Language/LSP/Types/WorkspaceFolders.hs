{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.LSP.Types.WorkspaceFolders where

import           Data.Aeson.TH
import           Data.Text                      ( Text )

import           Language.LSP.Types.Common
import           Language.LSP.Types.Utils

data WorkspaceFolder =
  WorkspaceFolder
    { -- | The URI of the workspace folder.
      _uri  :: Text
    -- | The name of the workspace folder. Defaults to the uri's basename.
    , _name :: Text
    } deriving (Read, Show, Eq)

deriveJSON lspOptions ''WorkspaceFolder

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

