{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.RenameClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.PrepareSupportDefaultBehavior
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data RenameClientCapabilities = RenameClientCapabilities 
  { {-|
  Whether rename supports dynamic registration.
  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  Client supports testing for validity of rename operations
  before execution.

  @since 3.12.0
  -}
  _prepareSupport :: (Maybe Bool)
  , {-|
  Client supports the default behavior result.

  The value indicates the default behavior used by the
  client.

  @since 3.16.0
  -}
  _prepareSupportDefaultBehavior :: (Maybe Language.LSP.Protocol.Internal.Types.PrepareSupportDefaultBehavior.PrepareSupportDefaultBehavior)
  , {-|
  Whether the client honors the change annotations in
  text edits and resource operations returned via the
  rename request's workspace edit by for example presenting
  the workspace edit in the user interface and asking
  for confirmation.

  @since 3.16.0
  -}
  _honorsChangeAnnotations :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON RenameClientCapabilities)

instance Aeson.ToJSON RenameClientCapabilities where
  toJSON (RenameClientCapabilities arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"prepareSupport" Language.LSP.Protocol.Types.Common..=? arg1
    ,"prepareSupportDefaultBehavior" Language.LSP.Protocol.Types.Common..=? arg2
    ,"honorsChangeAnnotations" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON RenameClientCapabilities where
  parseJSON = Aeson.withObject "RenameClientCapabilities" $ \arg -> RenameClientCapabilities <$> arg Aeson..:! "dynamicRegistration" <*> arg Aeson..:! "prepareSupport" <*> arg Aeson..:! "prepareSupportDefaultBehavior" <*> arg Aeson..:! "honorsChangeAnnotations"
