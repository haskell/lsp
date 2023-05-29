-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.Command where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
Represents a reference to a command. Provides a title which
will be used to represent a command in the UI and, optionally,
an array of arguments which will be passed to the command handler
function when invoked.

-}
data Command = Command 
  { {-|
  Title of the command, like `save`.

  -}
  _title :: Data.Text.Text
  , {-|
  The identifier of the actual command handler.

  -}
  _command :: Data.Text.Text
  , {-|
  Arguments that the command handler should be
  invoked with.

  -}
  _arguments :: (Maybe [Data.Aeson.Value])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON Command where
  toJSON (Command arg0 arg1 arg2) = Aeson.object $ concat $  [["title" Aeson..= arg0]
    ,["command" Aeson..= arg1]
    ,"arguments" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON Command where
  parseJSON = Aeson.withObject "Command" $ \arg -> Command <$> arg Aeson..: "title" <*> arg Aeson..: "command" <*> arg Aeson..:! "arguments"