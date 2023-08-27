{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ExecuteCommandParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
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
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ExecuteCommandParams)

instance Aeson.ToJSON ExecuteCommandParams where
  toJSON (ExecuteCommandParams arg0 arg1 arg2) = Aeson.object $ concat $  ["workDoneToken" Language.LSP.Protocol.Types.Common..=? arg0
    ,["command" Aeson..= arg1]
    ,"arguments" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON ExecuteCommandParams where
  parseJSON = Aeson.withObject "ExecuteCommandParams" $ \arg -> ExecuteCommandParams <$> arg Aeson..:! "workDoneToken" <*> arg Aeson..: "command" <*> arg Aeson..:! "arguments"
