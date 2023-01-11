-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ExecuteCommandParams where

import GHC.Generics
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameters of a `ExecuteCommandRequest`.

-}
data ExecuteCommandParams = ExecuteCommandParams 
  { {-|
  An optional token that a server can use to report work done progress.

  -}
  _workDoneToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|
  The identifier of the actual command handler.

  -}
  _command :: Data.Text.Text
  , {-|
  Arguments that the command should be invoked with.

  -}
  _arguments :: (Maybe [Data.Aeson.Value])
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON ExecuteCommandParams where
  toJSON (ExecuteCommandParams arg0 arg1 arg2) = Aeson.object $ concat $  ["workDoneToken" Language.LSP.Protocol.Types.Common..=? arg0
    ,["command" Aeson..= arg1]
    ,"arguments" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON ExecuteCommandParams where
  parseJSON = Aeson.withObject "ExecuteCommandParams" $ \arg -> ExecuteCommandParams <$> arg Aeson..:! "workDoneToken" <*> arg Aeson..: "command" <*> arg Aeson..:! "arguments"