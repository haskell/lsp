-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CreateFileOptions where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
Options to create a file.

-}
data CreateFileOptions = CreateFileOptions 
  { {-|
  Overwrite existing file. Overwrite wins over `ignoreIfExists`

  -}
  _overwrite :: (Maybe Bool)
  , {-|
  Ignore if exists.

  -}
  _ignoreIfExists :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON CreateFileOptions where
  toJSON (CreateFileOptions arg0 arg1) = Aeson.object $ concat $  ["overwrite" Language.LSP.Protocol.Types.Common..=? arg0
    ,"ignoreIfExists" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON CreateFileOptions where
  parseJSON = Aeson.withObject "CreateFileOptions" $ \arg -> CreateFileOptions <$> arg Aeson..:! "overwrite" <*> arg Aeson..:! "ignoreIfExists"