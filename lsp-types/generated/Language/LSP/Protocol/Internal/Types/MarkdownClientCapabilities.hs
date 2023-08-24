-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.MarkdownClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
Client capabilities specific to the used markdown parser.

@since 3.16.0
-}
data MarkdownClientCapabilities = MarkdownClientCapabilities 
  { {-|
  The name of the parser.
  -}
  _parser :: Data.Text.Text
  , {-|
  The version of the parser.
  -}
  _version :: (Maybe Data.Text.Text)
  , {-|
  A list of HTML tags that the client allows / supports in
  Markdown.

  @since 3.17.0
  -}
  _allowedTags :: (Maybe [Data.Text.Text])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON MarkdownClientCapabilities)

instance Aeson.ToJSON MarkdownClientCapabilities where
  toJSON (MarkdownClientCapabilities arg0 arg1 arg2) = Aeson.object $ concat $  [["parser" Aeson..= arg0]
    ,"version" Language.LSP.Protocol.Types.Common..=? arg1
    ,"allowedTags" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON MarkdownClientCapabilities where
  parseJSON = Aeson.withObject "MarkdownClientCapabilities" $ \arg -> MarkdownClientCapabilities <$> arg Aeson..: "parser" <*> arg Aeson..:! "version" <*> arg Aeson..:! "allowedTags"
