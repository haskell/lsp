{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CreateFilesParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.FileCreate
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameters sent in notifications/requests for user-initiated creation of
files.

@since 3.16.0
-}
data CreateFilesParams = CreateFilesParams 
  { {-|
  An array of all files/folders created in this operation.
  -}
  _files :: [Language.LSP.Protocol.Internal.Types.FileCreate.FileCreate]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CreateFilesParams)

instance Aeson.ToJSON CreateFilesParams where
  toJSON (CreateFilesParams arg0) = Aeson.object $ concat $  [["files" Aeson..= arg0]]

instance Aeson.FromJSON CreateFilesParams where
  parseJSON = Aeson.withObject "CreateFilesParams" $ \arg -> CreateFilesParams <$> arg Aeson..: "files"
