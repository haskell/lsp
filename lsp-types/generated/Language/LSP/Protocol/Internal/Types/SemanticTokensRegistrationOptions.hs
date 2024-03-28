{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SemanticTokensRegistrationOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.DocumentSelector
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensFullDelta
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensLegend
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.16.0
-}
data SemanticTokensRegistrationOptions = SemanticTokensRegistrationOptions 
  { {-|
  A document selector to identify the scope of the registration. If set to null
  the document selector provided on the client side will be used.
  -}
  documentSelector :: (Language.LSP.Protocol.Internal.Types.DocumentSelector.DocumentSelector Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|

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
  , {-|
  The id used to register the request. The id can be used to deregister
  the request again. See also Registration#id.
  -}
  id :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SemanticTokensRegistrationOptions)

instance Aeson.ToJSON SemanticTokensRegistrationOptions where
  toJSON (SemanticTokensRegistrationOptions arg0 arg1 arg2 arg3 arg4 arg5) = Aeson.object $ concat $  [["documentSelector" Aeson..= arg0]
    ,"workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg1
    ,["legend" Aeson..= arg2]
    ,"range" Language.LSP.Protocol.Types.Common..=? arg3
    ,"full" Language.LSP.Protocol.Types.Common..=? arg4
    ,"id" Language.LSP.Protocol.Types.Common..=? arg5]

instance Aeson.FromJSON SemanticTokensRegistrationOptions where
  parseJSON = Aeson.withObject "SemanticTokensRegistrationOptions" $ \arg -> SemanticTokensRegistrationOptions <$> arg Aeson..: "documentSelector" <*> arg Language.LSP.Protocol.Types.Common..:!? "workDoneProgress" <*> arg Aeson..: "legend" <*> arg Language.LSP.Protocol.Types.Common..:!? "range" <*> arg Language.LSP.Protocol.Types.Common..:!? "full" <*> arg Language.LSP.Protocol.Types.Common..:!? "id"
