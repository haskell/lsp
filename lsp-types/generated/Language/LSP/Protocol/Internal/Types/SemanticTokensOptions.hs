-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SemanticTokensOptions where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensLegend
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.16.0
-}
data SemanticTokensOptions = SemanticTokensOptions 
  { {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  , {-|
  The legend used by the server
  -}
  _legend :: Language.LSP.Protocol.Internal.Types.SemanticTokensLegend.SemanticTokensLegend
  , {-|
  Server supports providing semantic tokens for a specific range
  of a document.
  -}
  _range :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Row.Rec Row.Empty)))
  , {-|
  Server supports providing semantic tokens for a full document.
  -}
  _full :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Row.Rec ("delta" Row..== (Maybe Bool) Row..+ Row.Empty))))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON SemanticTokensOptions where
  toJSON (SemanticTokensOptions arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,["legend" Aeson..= arg1]
    ,"range" Language.LSP.Protocol.Types.Common..=? arg2
    ,"full" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON SemanticTokensOptions where
  parseJSON = Aeson.withObject "SemanticTokensOptions" $ \arg -> SemanticTokensOptions <$> arg Aeson..:! "workDoneProgress" <*> arg Aeson..: "legend" <*> arg Aeson..:! "range" <*> arg Aeson..:! "full"