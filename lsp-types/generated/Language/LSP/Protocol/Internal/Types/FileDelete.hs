-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FileDelete where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
Represents information on a file/folder delete.

@since 3.16.0

-}
data FileDelete = FileDelete 
  { {-|
  A file:// URI for the location of the file/folder being deleted.

  -}
  _uri :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON FileDelete where
  toJSON (FileDelete arg0) = Aeson.object $ concat $  [["uri" Aeson..= arg0]]

instance Aeson.FromJSON FileDelete where
  parseJSON = Aeson.withObject "FileDelete" $ \arg -> FileDelete <$> arg Aeson..: "uri"