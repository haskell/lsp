-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.RelativePattern where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.Pattern
import qualified Language.LSP.Protocol.Internal.Types.WorkspaceFolder
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
A relative pattern is a helper to construct glob patterns that are matched
relatively to a base URI. The common value for a `baseUri` is a workspace
folder root, but it can be another absolute URI as well.

@since 3.17.0

-}
data RelativePattern = RelativePattern 
  { {-|
  A workspace folder or a base URI to which this pattern will be matched
  against relatively.

  -}
  _baseUri :: (Language.LSP.Protocol.Internal.Types.WorkspaceFolder.WorkspaceFolder Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Uri.Uri)
  , {-|
  The actual glob pattern;

  -}
  _pattern :: Language.LSP.Protocol.Internal.Types.Pattern.Pattern
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON RelativePattern where
  toJSON (RelativePattern arg0 arg1) = Aeson.object $ concat $  [["baseUri" Aeson..= arg0]
    ,["pattern" Aeson..= arg1]]

instance Aeson.FromJSON RelativePattern where
  parseJSON = Aeson.withObject "RelativePattern" $ \arg -> RelativePattern <$> arg Aeson..: "baseUri" <*> arg Aeson..: "pattern"