{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.GeneralClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.MarkdownClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.PositionEncodingKind
import qualified Language.LSP.Protocol.Internal.Types.RegularExpressionsClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.StaleRequestSupportOptions
import qualified Language.LSP.Protocol.Types.Common

{-|
General client capabilities.

@since 3.16.0
-}
data GeneralClientCapabilities = GeneralClientCapabilities 
  { {-|
  Client capability that signals how the client
  handles stale requests (e.g. a request
  for which the client will not process the response
  anymore since the information is outdated).

  @since 3.17.0
  -}
  staleRequestSupport :: (Maybe Language.LSP.Protocol.Internal.Types.StaleRequestSupportOptions.StaleRequestSupportOptions)
  , {-|
  Client capabilities specific to regular expressions.

  @since 3.16.0
  -}
  regularExpressions :: (Maybe Language.LSP.Protocol.Internal.Types.RegularExpressionsClientCapabilities.RegularExpressionsClientCapabilities)
  , {-|
  Client capabilities specific to the client's markdown parser.

  @since 3.16.0
  -}
  markdown :: (Maybe Language.LSP.Protocol.Internal.Types.MarkdownClientCapabilities.MarkdownClientCapabilities)
  , {-|
  The position encodings supported by the client. Client and server
  have to agree on the same position encoding to ensure that offsets
  (e.g. character position in a line) are interpreted the same on both
  sides.

  To keep the protocol backwards compatible the following applies: if
  the value 'utf-16' is missing from the array of position encodings
  servers can assume that the client supports UTF-16. UTF-16 is
  therefore a mandatory encoding.

  If omitted it defaults to ['utf-16'].

  Implementation considerations: since the conversion from one encoding
  into another requires the content of the file / line the conversion
  is best done where the file is read which is usually on the server
  side.

  @since 3.17.0
  -}
  positionEncodings :: (Maybe [Language.LSP.Protocol.Internal.Types.PositionEncodingKind.PositionEncodingKind])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON GeneralClientCapabilities)

instance Aeson.ToJSON GeneralClientCapabilities where
  toJSON (GeneralClientCapabilities arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["staleRequestSupport" Language.LSP.Protocol.Types.Common..=? arg0
    ,"regularExpressions" Language.LSP.Protocol.Types.Common..=? arg1
    ,"markdown" Language.LSP.Protocol.Types.Common..=? arg2
    ,"positionEncodings" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON GeneralClientCapabilities where
  parseJSON = Aeson.withObject "GeneralClientCapabilities" $ \arg -> GeneralClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "staleRequestSupport" <*> arg Language.LSP.Protocol.Types.Common..:!? "regularExpressions" <*> arg Language.LSP.Protocol.Types.Common..:!? "markdown" <*> arg Language.LSP.Protocol.Types.Common..:!? "positionEncodings"
