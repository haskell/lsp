{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SemanticTokensOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensFullDelta
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensLegend
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.16.0
-}
data SemanticTokensOptions = SemanticTokensOptions 
  { {-|

  -}
  workDoneProgress :: (Maybe Bool)
  , {-|
  The legend used by the server
  -}
  legend :: Language.LSP.Protocol.Internal.Types.SemanticTokensLegend.SemanticTokensLegend
  , {-|
  Server supports providing semantic tokens for a specific range
  of a document.
  -}
  range :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Row.Rec Row.Empty)))
  , {-|
  Server supports providing semantic tokens for a full document.
  -}
  full :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.SemanticTokensFullDelta.SemanticTokensFullDelta))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SemanticTokensOptions)

instance Aeson.ToJSON SemanticTokensOptions where
  toJSON (SemanticTokensOptions arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,["legend" Aeson..= arg1]
    ,"range" Language.LSP.Protocol.Types.Common..=? arg2
    ,"full" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON SemanticTokensOptions where
  parseJSON = Aeson.withObject "SemanticTokensOptions" $ \arg -> SemanticTokensOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "workDoneProgress" <*> arg Aeson..: "legend" <*> arg Language.LSP.Protocol.Types.Common..:!? "range" <*> arg Language.LSP.Protocol.Types.Common..:!? "full"
