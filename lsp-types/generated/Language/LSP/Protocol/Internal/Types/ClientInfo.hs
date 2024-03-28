{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientInfo where

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
Information about the client

@since 3.15.0
@since 3.18.0 ClientInfo type name added.
@proposed
-}
data ClientInfo = ClientInfo 
  { {-|
  The name of the client as defined by the client.
  -}
  name :: Data.Text.Text
  , {-|
  The client's version as defined by the client.
  -}
  version :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientInfo)

instance Aeson.ToJSON ClientInfo where
  toJSON (ClientInfo arg0 arg1) = Aeson.object $ concat $  [["name" Aeson..= arg0]
    ,"version" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON ClientInfo where
  parseJSON = Aeson.withObject "ClientInfo" $ \arg -> ClientInfo <$> arg Aeson..: "name" <*> arg Language.LSP.Protocol.Types.Common..:!? "version"
