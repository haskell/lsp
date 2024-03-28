{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.RegistrationParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.Registration
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data RegistrationParams = RegistrationParams 
  { {-|

  -}
  registrations :: [Language.LSP.Protocol.Internal.Types.Registration.Registration]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON RegistrationParams)

instance Aeson.ToJSON RegistrationParams where
  toJSON (RegistrationParams arg0) = Aeson.object $ concat $  [["registrations" Aeson..= arg0]]

instance Aeson.FromJSON RegistrationParams where
  parseJSON = Aeson.withObject "RegistrationParams" $ \arg -> RegistrationParams <$> arg Aeson..: "registrations"
