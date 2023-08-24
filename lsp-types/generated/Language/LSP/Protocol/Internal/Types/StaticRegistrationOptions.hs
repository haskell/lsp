{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.StaticRegistrationOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
Static registration options to be returned in the initialize
request.
-}
data StaticRegistrationOptions = StaticRegistrationOptions 
  { {-|
  The id used to register the request. The id can be used to deregister
  the request again. See also Registration#id.
  -}
  _id :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON StaticRegistrationOptions)

instance Aeson.ToJSON StaticRegistrationOptions where
  toJSON (StaticRegistrationOptions arg0) = Aeson.object $ concat $  ["id" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON StaticRegistrationOptions where
  parseJSON = Aeson.withObject "StaticRegistrationOptions" $ \arg -> StaticRegistrationOptions <$> arg Aeson..:! "id"
