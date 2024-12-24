{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.UnregistrationParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.Unregistration
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data UnregistrationParams = UnregistrationParams 
  { {-|

  -}
  unregisterations :: [Language.LSP.Protocol.Internal.Types.Unregistration.Unregistration]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON UnregistrationParams)

instance Aeson.ToJSON UnregistrationParams where
  toJSON (UnregistrationParams arg0) = Aeson.object $ concat $  [["unregisterations" Aeson..= arg0]]

instance Aeson.FromJSON UnregistrationParams where
  parseJSON = Aeson.withObject "UnregistrationParams" $ \arg -> UnregistrationParams <$> arg Aeson..: "unregisterations"
