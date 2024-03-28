{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.Position where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common

{-|
Position in a text document expressed as zero-based line and character
offset. Prior to 3.17 the offsets were always based on a UTF-16 string
representation. So a string of the form `a𐐀b` the character offset of the
character `a` is 0, the character offset of `𐐀` is 1 and the character
offset of b is 3 since `𐐀` is represented using two code units in UTF-16.
Since 3.17 clients and servers can agree on a different string encoding
representation (e.g. UTF-8). The client announces it's supported encoding
via the client capability [`general.positionEncodings`](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#clientCapabilities).
The value is an array of position encodings the client supports, with
decreasing preference (e.g. the encoding at index `0` is the most preferred
one). To stay backwards compatible the only mandatory encoding is UTF-16
represented via the string `utf-16`. The server can pick one of the
encodings offered by the client and signals that encoding back to the
client via the initialize result's property
[`capabilities.positionEncoding`](https://microsoft.github.io/language-server-protocol/specifications/specification-current/#serverCapabilities). If the string value
`utf-16` is missing from the client's capability `general.positionEncodings`
servers can safely assume that the client supports UTF-16. If the server
omits the position encoding in its initialize result the encoding defaults
to the string value `utf-16`. Implementation considerations: since the
conversion from one encoding into another requires the content of the
file / line the conversion is best done where the file is read which is
usually on the server side.

Positions are line end character agnostic. So you can not specify a position
that denotes `\r|\n` or `\n|` where `|` represents the character offset.

@since 3.17.0 - support for negotiated position encoding.
-}
data Position = Position 
  { {-|
  Line position in a document (zero-based).

  If a line number is greater than the number of lines in a document, it defaults back to the number of lines in the document.
  If a line number is negative, it defaults to 0.
  -}
  line :: Language.LSP.Protocol.Types.Common.UInt
  , {-|
  Character offset on a line in a document (zero-based).

  The meaning of this offset is determined by the negotiated
  `PositionEncodingKind`.

  If the character value is greater than the line length it defaults back to the
  line length.
  -}
  character :: Language.LSP.Protocol.Types.Common.UInt
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON Position)

instance Aeson.ToJSON Position where
  toJSON (Position arg0 arg1) = Aeson.object $ concat $  [["line" Aeson..= arg0]
    ,["character" Aeson..= arg1]]

instance Aeson.FromJSON Position where
  parseJSON = Aeson.withObject "Position" $ \arg -> Position <$> arg Aeson..: "line" <*> arg Aeson..: "character"
