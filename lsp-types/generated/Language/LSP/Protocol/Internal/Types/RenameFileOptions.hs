{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.RenameFileOptions where

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
Rename file options
-}
data RenameFileOptions = RenameFileOptions 
  { {-|
  Overwrite target if existing. Overwrite wins over `ignoreIfExists`
  -}
  _overwrite :: (Maybe Bool)
  , {-|
  Ignores if target exists.
  -}
  _ignoreIfExists :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON RenameFileOptions)

instance Aeson.ToJSON RenameFileOptions where
  toJSON (RenameFileOptions arg0 arg1) = Aeson.object $ concat $  ["overwrite" Language.LSP.Protocol.Types.Common..=? arg0
    ,"ignoreIfExists" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON RenameFileOptions where
  parseJSON = Aeson.withObject "RenameFileOptions" $ \arg -> RenameFileOptions <$> arg Aeson..:! "overwrite" <*> arg Aeson..:! "ignoreIfExists"
