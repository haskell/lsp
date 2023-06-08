-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CodeActionRegistrationOptions where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.CodeActionKind
import qualified Language.LSP.Protocol.Internal.Types.DocumentSelector
import qualified Language.LSP.Protocol.Types.Common

{-|
Registration options for a `CodeActionRequest`.
-}
data CodeActionRegistrationOptions = CodeActionRegistrationOptions 
  { {-|
  A document selector to identify the scope of the registration. If set to null
  the document selector provided on the client side will be used.
  -}
  _documentSelector :: (Language.LSP.Protocol.Internal.Types.DocumentSelector.DocumentSelector Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  , {-|
  CodeActionKinds that this server may return.

  The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
  may list out every specific kind they provide.
  -}
  _codeActionKinds :: (Maybe [Language.LSP.Protocol.Internal.Types.CodeActionKind.CodeActionKind])
  , {-|
  The server provides support to resolve additional
  information for a code action.

  @since 3.16.0
  -}
  _resolveProvider :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON CodeActionRegistrationOptions where
  toJSON (CodeActionRegistrationOptions arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["documentSelector" Aeson..= arg0]
    ,"workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg1
    ,"codeActionKinds" Language.LSP.Protocol.Types.Common..=? arg2
    ,"resolveProvider" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON CodeActionRegistrationOptions where
  parseJSON = Aeson.withObject "CodeActionRegistrationOptions" $ \arg -> CodeActionRegistrationOptions <$> arg Aeson..: "documentSelector" <*> arg Aeson..:! "workDoneProgress" <*> arg Aeson..:! "codeActionKinds" <*> arg Aeson..:! "resolveProvider"
