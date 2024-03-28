{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CreateFile where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier
import qualified Language.LSP.Protocol.Internal.Types.CreateFileOptions
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Singletons
import qualified Language.LSP.Protocol.Types.Uri

{-|
Create file operation.
-}
data CreateFile = CreateFile 
  { {-|
  An optional annotation identifier describing the operation.

  @since 3.16.0
  -}
  annotationId :: (Maybe Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier.ChangeAnnotationIdentifier)
  , {-|
  A create
  -}
  kind :: (Language.LSP.Protocol.Types.Singletons.AString "create")
  , {-|
  The resource to create.
  -}
  uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  Additional options
  -}
  options :: (Maybe Language.LSP.Protocol.Internal.Types.CreateFileOptions.CreateFileOptions)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CreateFile)

instance Aeson.ToJSON CreateFile where
  toJSON (CreateFile arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["annotationId" Language.LSP.Protocol.Types.Common..=? arg0
    ,["kind" Aeson..= arg1]
    ,["uri" Aeson..= arg2]
    ,"options" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON CreateFile where
  parseJSON = Aeson.withObject "CreateFile" $ \arg -> CreateFile <$> arg Language.LSP.Protocol.Types.Common..:!? "annotationId" <*> arg Aeson..: "kind" <*> arg Aeson..: "uri" <*> arg Language.LSP.Protocol.Types.Common..:!? "options"
