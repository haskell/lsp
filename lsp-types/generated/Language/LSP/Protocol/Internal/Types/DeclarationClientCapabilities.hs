{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DeclarationClientCapabilities where

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
@since 3.14.0
-}
data DeclarationClientCapabilities = DeclarationClientCapabilities 
  { {-|
  Whether declaration supports dynamic registration. If this is set to `true`
  the client supports the new `DeclarationRegistrationOptions` return value
  for the corresponding server capability as well.
  -}
  dynamicRegistration :: (Maybe Bool)
  , {-|
  The client supports additional metadata in the form of declaration links.
  -}
  linkSupport :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DeclarationClientCapabilities)

instance Aeson.ToJSON DeclarationClientCapabilities where
  toJSON (DeclarationClientCapabilities arg0 arg1) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"linkSupport" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON DeclarationClientCapabilities where
  parseJSON = Aeson.withObject "DeclarationClientCapabilities" $ \arg -> DeclarationClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "dynamicRegistration" <*> arg Language.LSP.Protocol.Types.Common..:!? "linkSupport"
