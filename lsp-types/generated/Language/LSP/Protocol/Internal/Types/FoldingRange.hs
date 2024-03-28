{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FoldingRange where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.FoldingRangeKind
import qualified Language.LSP.Protocol.Types.Common

{-|
Represents a folding range. To be valid, start and end line must be bigger than zero and smaller
than the number of lines in the document. Clients are free to ignore invalid ranges.
-}
data FoldingRange = FoldingRange 
  { {-|
  The zero-based start line of the range to fold. The folded area starts after the line's last character.
  To be valid, the end must be zero or larger and smaller than the number of lines in the document.
  -}
  startLine :: Language.LSP.Protocol.Types.Common.UInt
  , {-|
  The zero-based character offset from where the folded range starts. If not defined, defaults to the length of the start line.
  -}
  startCharacter :: (Maybe Language.LSP.Protocol.Types.Common.UInt)
  , {-|
  The zero-based end line of the range to fold. The folded area ends with the line's last character.
  To be valid, the end must be zero or larger and smaller than the number of lines in the document.
  -}
  endLine :: Language.LSP.Protocol.Types.Common.UInt
  , {-|
  The zero-based character offset before the folded range ends. If not defined, defaults to the length of the end line.
  -}
  endCharacter :: (Maybe Language.LSP.Protocol.Types.Common.UInt)
  , {-|
  Describes the kind of the folding range such as 'comment' or 'region'. The kind
  is used to categorize folding ranges and used by commands like 'Fold all comments'.
  See `FoldingRangeKind` for an enumeration of standardized kinds.
  -}
  kind :: (Maybe Language.LSP.Protocol.Internal.Types.FoldingRangeKind.FoldingRangeKind)
  , {-|
  The text that the client should show when the specified range is
  collapsed. If not defined or not supported by the client, a default
  will be chosen by the client.

  @since 3.17.0
  -}
  collapsedText :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON FoldingRange)

instance Aeson.ToJSON FoldingRange where
  toJSON (FoldingRange arg0 arg1 arg2 arg3 arg4 arg5) = Aeson.object $ concat $  [["startLine" Aeson..= arg0]
    ,"startCharacter" Language.LSP.Protocol.Types.Common..=? arg1
    ,["endLine" Aeson..= arg2]
    ,"endCharacter" Language.LSP.Protocol.Types.Common..=? arg3
    ,"kind" Language.LSP.Protocol.Types.Common..=? arg4
    ,"collapsedText" Language.LSP.Protocol.Types.Common..=? arg5]

instance Aeson.FromJSON FoldingRange where
  parseJSON = Aeson.withObject "FoldingRange" $ \arg -> FoldingRange <$> arg Aeson..: "startLine" <*> arg Language.LSP.Protocol.Types.Common..:!? "startCharacter" <*> arg Aeson..: "endLine" <*> arg Language.LSP.Protocol.Types.Common..:!? "endCharacter" <*> arg Language.LSP.Protocol.Types.Common..:!? "kind" <*> arg Language.LSP.Protocol.Types.Common..:!? "collapsedText"
