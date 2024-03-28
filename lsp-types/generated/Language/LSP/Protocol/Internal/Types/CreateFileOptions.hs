{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CreateFileOptions where

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
Options to create a file.
-}
data CreateFileOptions = CreateFileOptions 
  { {-|
  Overwrite existing file. Overwrite wins over `ignoreIfExists`
  -}
  overwrite :: (Maybe Bool)
  , {-|
  Ignore if exists.
  -}
  ignoreIfExists :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CreateFileOptions)

instance Aeson.ToJSON CreateFileOptions where
  toJSON (CreateFileOptions arg0 arg1) = Aeson.object $ concat $  ["overwrite" Language.LSP.Protocol.Types.Common..=? arg0
    ,"ignoreIfExists" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON CreateFileOptions where
  parseJSON = Aeson.withObject "CreateFileOptions" $ \arg -> CreateFileOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "overwrite" <*> arg Language.LSP.Protocol.Types.Common..:!? "ignoreIfExists"
