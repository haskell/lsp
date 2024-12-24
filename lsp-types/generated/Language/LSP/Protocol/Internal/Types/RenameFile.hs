{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.RenameFile where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier
import qualified Language.LSP.Protocol.Internal.Types.RenameFileOptions
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Singletons
import qualified Language.LSP.Protocol.Types.Uri

{-|
Rename file operation
-}
data RenameFile = RenameFile 
  { {-|
  An optional annotation identifier describing the operation.

  @since 3.16.0
  -}
  annotationId :: (Maybe Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier.ChangeAnnotationIdentifier)
  , {-|
  A rename
  -}
  kind :: (Language.LSP.Protocol.Types.Singletons.AString "rename")
  , {-|
  The old (existing) location.
  -}
  oldUri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  The new location.
  -}
  newUri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  Rename options.
  -}
  options :: (Maybe Language.LSP.Protocol.Internal.Types.RenameFileOptions.RenameFileOptions)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON RenameFile)

instance Aeson.ToJSON RenameFile where
  toJSON (RenameFile arg0 arg1 arg2 arg3 arg4) = Aeson.object $ concat $  ["annotationId" Language.LSP.Protocol.Types.Common..=? arg0
    ,["kind" Aeson..= arg1]
    ,["oldUri" Aeson..= arg2]
    ,["newUri" Aeson..= arg3]
    ,"options" Language.LSP.Protocol.Types.Common..=? arg4]

instance Aeson.FromJSON RenameFile where
  parseJSON = Aeson.withObject "RenameFile" $ \arg -> RenameFile <$> arg Language.LSP.Protocol.Types.Common..:!? "annotationId" <*> arg Aeson..: "kind" <*> arg Aeson..: "oldUri" <*> arg Aeson..: "newUri" <*> arg Language.LSP.Protocol.Types.Common..:!? "options"
