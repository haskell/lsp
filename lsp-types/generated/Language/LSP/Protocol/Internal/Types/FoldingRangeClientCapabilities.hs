{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FoldingRangeClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ClientFoldingRangeKindOptions
import qualified Language.LSP.Protocol.Internal.Types.ClientFoldingRangeOptions
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data FoldingRangeClientCapabilities = FoldingRangeClientCapabilities 
  { {-|
  Whether implementation supports dynamic registration for folding range
  providers. If this is set to `true` the client supports the new
  `FoldingRangeRegistrationOptions` return value for the corresponding
  server capability as well.
  -}
  dynamicRegistration :: (Maybe Bool)
  , {-|
  The maximum number of folding ranges that the client prefers to receive
  per document. The value serves as a hint, servers are free to follow the
  limit.
  -}
  rangeLimit :: (Maybe Language.LSP.Protocol.Types.Common.UInt)
  , {-|
  If set, the client signals that it only supports folding complete lines.
  If set, client will ignore specified `startCharacter` and `endCharacter`
  properties in a FoldingRange.
  -}
  lineFoldingOnly :: (Maybe Bool)
  , {-|
  Specific options for the folding range kind.

  @since 3.17.0
  -}
  foldingRangeKind :: (Maybe Language.LSP.Protocol.Internal.Types.ClientFoldingRangeKindOptions.ClientFoldingRangeKindOptions)
  , {-|
  Specific options for the folding range.

  @since 3.17.0
  -}
  foldingRange :: (Maybe Language.LSP.Protocol.Internal.Types.ClientFoldingRangeOptions.ClientFoldingRangeOptions)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON FoldingRangeClientCapabilities)

instance Aeson.ToJSON FoldingRangeClientCapabilities where
  toJSON (FoldingRangeClientCapabilities arg0 arg1 arg2 arg3 arg4) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"rangeLimit" Language.LSP.Protocol.Types.Common..=? arg1
    ,"lineFoldingOnly" Language.LSP.Protocol.Types.Common..=? arg2
    ,"foldingRangeKind" Language.LSP.Protocol.Types.Common..=? arg3
    ,"foldingRange" Language.LSP.Protocol.Types.Common..=? arg4]

instance Aeson.FromJSON FoldingRangeClientCapabilities where
  parseJSON = Aeson.withObject "FoldingRangeClientCapabilities" $ \arg -> FoldingRangeClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "dynamicRegistration" <*> arg Language.LSP.Protocol.Types.Common..:!? "rangeLimit" <*> arg Language.LSP.Protocol.Types.Common..:!? "lineFoldingOnly" <*> arg Language.LSP.Protocol.Types.Common..:!? "foldingRangeKind" <*> arg Language.LSP.Protocol.Types.Common..:!? "foldingRange"
