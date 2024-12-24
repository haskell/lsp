{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InitializeResult where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ServerCapabilities
import qualified Language.LSP.Protocol.Internal.Types.ServerInfo
import qualified Language.LSP.Protocol.Types.Common

{-|
The result returned from an initialize request.
-}
data InitializeResult = InitializeResult 
  { {-|
  The capabilities the language server provides.
  -}
  capabilities :: Language.LSP.Protocol.Internal.Types.ServerCapabilities.ServerCapabilities
  , {-|
  Information about the server.

  @since 3.15.0
  -}
  serverInfo :: (Maybe Language.LSP.Protocol.Internal.Types.ServerInfo.ServerInfo)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON InitializeResult)

instance Aeson.ToJSON InitializeResult where
  toJSON (InitializeResult arg0 arg1) = Aeson.object $ concat $  [["capabilities" Aeson..= arg0]
    ,"serverInfo" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON InitializeResult where
  parseJSON = Aeson.withObject "InitializeResult" $ \arg -> InitializeResult <$> arg Aeson..: "capabilities" <*> arg Language.LSP.Protocol.Types.Common..:!? "serverInfo"
