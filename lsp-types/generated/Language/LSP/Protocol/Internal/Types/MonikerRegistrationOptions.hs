{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.MonikerRegistrationOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.DocumentSelector
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data MonikerRegistrationOptions = MonikerRegistrationOptions 
  { {-|
  A document selector to identify the scope of the registration. If set to null
  the document selector provided on the client side will be used.
  -}
  _documentSelector :: (Language.LSP.Protocol.Internal.Types.DocumentSelector.DocumentSelector Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON MonikerRegistrationOptions)

instance Aeson.ToJSON MonikerRegistrationOptions where
  toJSON (MonikerRegistrationOptions arg0 arg1) = Aeson.object $ concat $  [["documentSelector" Aeson..= arg0]
    ,"workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON MonikerRegistrationOptions where
  parseJSON = Aeson.withObject "MonikerRegistrationOptions" $ \arg -> MonikerRegistrationOptions <$> arg Aeson..: "documentSelector" <*> arg Language.LSP.Protocol.Types.Common..:!? "workDoneProgress"
