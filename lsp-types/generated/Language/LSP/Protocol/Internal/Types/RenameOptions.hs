-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.RenameOptions where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
Provider options for a `RenameRequest`.

-}
data RenameOptions = RenameOptions 
  { {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  , {-|
  Renames should be checked and tested before being executed.

  @since version 3.12.0

  -}
  _prepareProvider :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON RenameOptions where
  toJSON (RenameOptions arg0 arg1) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,"prepareProvider" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON RenameOptions where
  parseJSON = Aeson.withObject "RenameOptions" $ \arg -> RenameOptions <$> arg Aeson..:! "workDoneProgress" <*> arg Aeson..:! "prepareProvider"