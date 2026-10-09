{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CodeLensClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ClientCodeLensResolveOptions
import qualified Language.LSP.Protocol.Types.Common

{-|
The client capabilities  of a `CodeLensRequest`.
-}
data CodeLensClientCapabilities = CodeLensClientCapabilities 
  { {-|
  Whether code lens supports dynamic registration.
  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  Whether the client supports resolving additional code lens
  properties via a separate `codeLens/resolve` request.

  @since 3.18.0
  -}
  _resolveSupport :: (Maybe Language.LSP.Protocol.Internal.Types.ClientCodeLensResolveOptions.ClientCodeLensResolveOptions)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CodeLensClientCapabilities)

instance Aeson.ToJSON CodeLensClientCapabilities where
  toJSON (CodeLensClientCapabilities arg0 arg1) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"resolveSupport" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON CodeLensClientCapabilities where
  parseJSON = Aeson.withObject "CodeLensClientCapabilities" $ \arg -> CodeLensClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "dynamicRegistration" <*> arg Language.LSP.Protocol.Types.Common..:!? "resolveSupport"
