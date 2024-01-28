{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ServerInfo where

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
Information about the server

@since 3.15.0
@since 3.18.0 ServerInfo type name added.
@proposed
-}
data ServerInfo = ServerInfo 
  { {-|
  The name of the server as defined by the server.
  -}
  _name :: Data.Text.Text
  , {-|
  The server's version as defined by the server.
  -}
  _version :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ServerInfo)

instance Aeson.ToJSON ServerInfo where
  toJSON (ServerInfo arg0 arg1) = Aeson.object $ concat $  [["name" Aeson..= arg0]
    ,"version" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON ServerInfo where
  parseJSON = Aeson.withObject "ServerInfo" $ \arg -> ServerInfo <$> arg Aeson..: "name" <*> arg Language.LSP.Protocol.Types.Common..:!? "version"
