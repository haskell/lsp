{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DuplicateRecordFields      #-}

module Language.LSP.Types.Command where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text
import           Language.LSP.Types.Common
import           Language.LSP.Types.Progress
import           Language.LSP.Types.Utils

-- -------------------------------------

data ExecuteCommandClientCapabilities =
  ExecuteCommandClientCapabilities
    { _dynamicRegistration :: Maybe Bool -- ^Execute command supports dynamic
                                         -- registration.
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ExecuteCommandClientCapabilities

-- -------------------------------------

makeExtendingDatatype "ExecuteCommandOptions" [''WorkDoneProgressOptions]
  [("_commands", [t| List Text |])]
deriveJSON lspOptions ''ExecuteCommandOptions

makeExtendingDatatype "ExecuteCommandRegistrationOptions" [''ExecuteCommandOptions] []
deriveJSON lspOptions ''ExecuteCommandRegistrationOptions

-- -------------------------------------

makeExtendingDatatype "ExecuteCommandParams" [''WorkDoneProgressParams]
  [ ("_command", [t| Text |])
  , ("_arguments", [t| Maybe (List Value) |])
  ]
deriveJSON lspOptions ''ExecuteCommandParams

data Command =
  Command
    { -- | Title of the command, like @save@.
      _title     :: Text
    , -- | The identifier of the actual command handler.
      _command   :: Text
    , -- | Arguments that the command handler should be invoked with.
      _arguments :: Maybe (List Value)
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''Command

