{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ClientShowMessageActionItemOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.18.0
@proposed
-}
data ClientShowMessageActionItemOptions = ClientShowMessageActionItemOptions 
  { {-|
  Whether the client supports additional attributes which
  are preserved and send back to the server in the
  request's response.
  -}
  _additionalPropertiesSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ClientShowMessageActionItemOptions)

instance Aeson.ToJSON ClientShowMessageActionItemOptions where
  toJSON (ClientShowMessageActionItemOptions arg0) = Aeson.object $ concat $  ["additionalPropertiesSupport" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON ClientShowMessageActionItemOptions where
  parseJSON = Aeson.withObject "ClientShowMessageActionItemOptions" $ \arg -> ClientShowMessageActionItemOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "additionalPropertiesSupport"
