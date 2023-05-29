-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CodeActionClientCapabilities where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.CodeActionKind
import qualified Language.LSP.Protocol.Types.Common

{-|
The Client Capabilities of a `CodeActionRequest`.

-}
data CodeActionClientCapabilities = CodeActionClientCapabilities 
  { {-|
  Whether code action supports dynamic registration.

  -}
  _dynamicRegistration :: (Maybe Bool)
  , {-|
  The client support code action literals of type `CodeAction` as a valid
  response of the `textDocument/codeAction` request. If the property is not
  set the request can only return `Command` literals.

  @since 3.8.0

  -}
  _codeActionLiteralSupport :: (Maybe (Row.Rec ("codeActionKind" Row..== (Row.Rec ("valueSet" Row..== [Language.LSP.Protocol.Internal.Types.CodeActionKind.CodeActionKind] Row..+ Row.Empty)) Row..+ Row.Empty)))
  , {-|
  Whether code action supports the `isPreferred` property.

  @since 3.15.0

  -}
  _isPreferredSupport :: (Maybe Bool)
  , {-|
  Whether code action supports the `disabled` property.

  @since 3.16.0

  -}
  _disabledSupport :: (Maybe Bool)
  , {-|
  Whether code action supports the `data` property which is
  preserved between a `textDocument/codeAction` and a
  `codeAction/resolve` request.

  @since 3.16.0

  -}
  _dataSupport :: (Maybe Bool)
  , {-|
  Whether the client supports resolving additional code action
  properties via a separate `codeAction/resolve` request.

  @since 3.16.0

  -}
  _resolveSupport :: (Maybe (Row.Rec ("properties" Row..== [Data.Text.Text] Row..+ Row.Empty)))
  , {-|
  Whether the client honors the change annotations in
  text edits and resource operations returned via the
  `CodeAction#edit` property by for example presenting
  the workspace edit in the user interface and asking
  for confirmation.

  @since 3.16.0

  -}
  _honorsChangeAnnotations :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON CodeActionClientCapabilities where
  toJSON (CodeActionClientCapabilities arg0 arg1 arg2 arg3 arg4 arg5 arg6) = Aeson.object $ concat $  ["dynamicRegistration" Language.LSP.Protocol.Types.Common..=? arg0
    ,"codeActionLiteralSupport" Language.LSP.Protocol.Types.Common..=? arg1
    ,"isPreferredSupport" Language.LSP.Protocol.Types.Common..=? arg2
    ,"disabledSupport" Language.LSP.Protocol.Types.Common..=? arg3
    ,"dataSupport" Language.LSP.Protocol.Types.Common..=? arg4
    ,"resolveSupport" Language.LSP.Protocol.Types.Common..=? arg5
    ,"honorsChangeAnnotations" Language.LSP.Protocol.Types.Common..=? arg6]

instance Aeson.FromJSON CodeActionClientCapabilities where
  parseJSON = Aeson.withObject "CodeActionClientCapabilities" $ \arg -> CodeActionClientCapabilities <$> arg Aeson..:! "dynamicRegistration" <*> arg Aeson..:! "codeActionLiteralSupport" <*> arg Aeson..:! "isPreferredSupport" <*> arg Aeson..:! "disabledSupport" <*> arg Aeson..:! "dataSupport" <*> arg Aeson..:! "resolveSupport" <*> arg Aeson..:! "honorsChangeAnnotations"