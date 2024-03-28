{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ExecuteCommandOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
The server capabilities of a `ExecuteCommandRequest`.
-}
data ExecuteCommandOptions = ExecuteCommandOptions 
  { {-|

  -}
  workDoneProgress :: (Maybe Bool)
  , {-|
  The commands to be executed on the server
  -}
  commands :: [Data.Text.Text]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ExecuteCommandOptions)

instance Aeson.ToJSON ExecuteCommandOptions where
  toJSON (ExecuteCommandOptions arg0 arg1) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,["commands" Aeson..= arg1]]

instance Aeson.FromJSON ExecuteCommandOptions where
  parseJSON = Aeson.withObject "ExecuteCommandOptions" $ \arg -> ExecuteCommandOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "workDoneProgress" <*> arg Aeson..: "commands"
