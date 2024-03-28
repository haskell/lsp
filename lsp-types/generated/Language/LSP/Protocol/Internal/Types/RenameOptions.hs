{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.RenameOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common

{-|
Provider options for a `RenameRequest`.
-}
data RenameOptions = RenameOptions 
  { {-|

  -}
  workDoneProgress :: (Maybe Bool)
  , {-|
  Renames should be checked and tested before being executed.

  @since version 3.12.0
  -}
  prepareProvider :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON RenameOptions)

instance Aeson.ToJSON RenameOptions where
  toJSON (RenameOptions arg0 arg1) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,"prepareProvider" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON RenameOptions where
  parseJSON = Aeson.withObject "RenameOptions" $ \arg -> RenameOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "workDoneProgress" <*> arg Language.LSP.Protocol.Types.Common..:!? "prepareProvider"
