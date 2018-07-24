{-# LANGUAGE TemplateHaskell            #-}

module Language.Haskell.LSP.TH.Command where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text
import           Language.Haskell.LSP.TH.Constants
import           Language.Haskell.LSP.TH.List
-- ---------------------------------------------------------------------
{-
Command

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#command

Represents a reference to a command. Provides a title which will be used to
represent a command in the UI. Commands are identitifed using a string
identifier and the protocol currently doesn't specify a set of well known
commands. So executing a command requires some tool extension code.

interface Command {
    /**
     * Title of the command, like `save`.
     */
    title: string;
    /**
     * The identifier of the actual command handler.
     */
    command: string;
    /**
     * Arguments that the command handler should be
     * invoked with.
     */
    arguments?: any[];
}
-}

data Command =
  Command
    { _title     :: Text
    , _command   :: Text
    , _arguments :: Maybe (List Value)
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''Command
