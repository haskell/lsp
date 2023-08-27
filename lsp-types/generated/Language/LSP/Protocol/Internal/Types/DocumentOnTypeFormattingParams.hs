{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.FormattingOptions
import qualified Language.LSP.Protocol.Internal.Types.Position
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameters of a `DocumentOnTypeFormattingRequest`.
-}
data DocumentOnTypeFormattingParams = DocumentOnTypeFormattingParams 
  { {-|
  The document to format.
  -}
  _textDocument :: Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier
  , {-|
  The position around which the on type formatting should happen.
  This is not necessarily the exact position where the character denoted
  by the property `ch` got typed.
  -}
  _position :: Language.LSP.Protocol.Internal.Types.Position.Position
  , {-|
  The character that has been typed that triggered the formatting
  on type request. That is not necessarily the last character that
  got inserted into the document since the client could auto insert
  characters as well (e.g. like automatic brace completion).
  -}
  _ch :: Data.Text.Text
  , {-|
  The formatting options.
  -}
  _options :: Language.LSP.Protocol.Internal.Types.FormattingOptions.FormattingOptions
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DocumentOnTypeFormattingParams)

instance Aeson.ToJSON DocumentOnTypeFormattingParams where
  toJSON (DocumentOnTypeFormattingParams arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["textDocument" Aeson..= arg0]
    ,["position" Aeson..= arg1]
    ,["ch" Aeson..= arg2]
    ,["options" Aeson..= arg3]]

instance Aeson.FromJSON DocumentOnTypeFormattingParams where
  parseJSON = Aeson.withObject "DocumentOnTypeFormattingParams" $ \arg -> DocumentOnTypeFormattingParams <$> arg Aeson..: "textDocument" <*> arg Aeson..: "position" <*> arg Aeson..: "ch" <*> arg Aeson..: "options"
