-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.RenameRegistrationOptions where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.DocumentSelector
import qualified Language.LSP.Protocol.Types.Common

{-|
Registration options for a `RenameRequest`.

-}
data RenameRegistrationOptions = RenameRegistrationOptions 
  { {-|
  A document selector to identify the scope of the registration. If set to null
  the document selector provided on the client side will be used.

  -}
  _documentSelector :: (Language.LSP.Protocol.Internal.Types.DocumentSelector.DocumentSelector Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  , {-|
  Renames should be checked and tested before being executed.

  @since version 3.12.0

  -}
  _prepareProvider :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON RenameRegistrationOptions where
  toJSON (RenameRegistrationOptions arg0 arg1 arg2) = Aeson.object $ concat $  [["documentSelector" Aeson..= arg0]
    ,"workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg1
    ,"prepareProvider" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON RenameRegistrationOptions where
  parseJSON = Aeson.withObject "RenameRegistrationOptions" $ \arg -> RenameRegistrationOptions <$> arg Aeson..: "documentSelector" <*> arg Aeson..:! "workDoneProgress" <*> arg Aeson..:! "prepareProvider"