-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DiagnosticRegistrationOptions where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.DocumentSelector
import qualified Language.LSP.Protocol.Types.Common

{-|
Diagnostic registration options.

@since 3.17.0

-}
data DiagnosticRegistrationOptions = DiagnosticRegistrationOptions 
  { {-|
  A document selector to identify the scope of the registration. If set to null
  the document selector provided on the client side will be used.

  -}
  _documentSelector :: (Language.LSP.Protocol.Internal.Types.DocumentSelector.DocumentSelector Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  , {-|
  An optional identifier under which the diagnostics are
  managed by the client.

  -}
  _identifier :: (Maybe Data.Text.Text)
  , {-|
  Whether the language has inter file dependencies meaning that
  editing code in one file can result in a different diagnostic
  set in another file. Inter file dependencies are common for
  most programming languages and typically uncommon for linters.

  -}
  _interFileDependencies :: Bool
  , {-|
  The server provides support for workspace diagnostics as well.

  -}
  _workspaceDiagnostics :: Bool
  , {-|
  The id used to register the request. The id can be used to deregister
  the request again. See also Registration#id.

  -}
  _id :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON DiagnosticRegistrationOptions where
  toJSON (DiagnosticRegistrationOptions arg0 arg1 arg2 arg3 arg4 arg5) = Aeson.object $ concat $  [["documentSelector" Aeson..= arg0]
    ,"workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg1
    ,"identifier" Language.LSP.Protocol.Types.Common..=? arg2
    ,["interFileDependencies" Aeson..= arg3]
    ,["workspaceDiagnostics" Aeson..= arg4]
    ,"id" Language.LSP.Protocol.Types.Common..=? arg5]

instance Aeson.FromJSON DiagnosticRegistrationOptions where
  parseJSON = Aeson.withObject "DiagnosticRegistrationOptions" $ \arg -> DiagnosticRegistrationOptions <$> arg Aeson..: "documentSelector" <*> arg Aeson..:! "workDoneProgress" <*> arg Aeson..:! "identifier" <*> arg Aeson..: "interFileDependencies" <*> arg Aeson..: "workspaceDiagnostics" <*> arg Aeson..:! "id"