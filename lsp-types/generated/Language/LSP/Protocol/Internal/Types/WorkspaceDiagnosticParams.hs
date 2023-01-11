-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceDiagnosticParams where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.PreviousResultId
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Types.Common

{-|
Parameters of the workspace diagnostic request.

@since 3.17.0

-}
data WorkspaceDiagnosticParams = WorkspaceDiagnosticParams 
  { {-|
  An optional token that a server can use to report work done progress.

  -}
  _workDoneToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|
  An optional token that a server can use to report partial results (e.g. streaming) to
  the client.

  -}
  _partialResultToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|
  The additional identifier provided during registration.

  -}
  _identifier :: (Maybe Data.Text.Text)
  , {-|
  The currently known diagnostic reports with their
  previous result ids.

  -}
  _previousResultIds :: [Language.LSP.Protocol.Internal.Types.PreviousResultId.PreviousResultId]
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON WorkspaceDiagnosticParams where
  toJSON (WorkspaceDiagnosticParams arg0 arg1 arg2 arg3) = Aeson.object $ concat $  ["workDoneToken" Language.LSP.Protocol.Types.Common..=? arg0
    ,"partialResultToken" Language.LSP.Protocol.Types.Common..=? arg1
    ,"identifier" Language.LSP.Protocol.Types.Common..=? arg2
    ,["previousResultIds" Aeson..= arg3]]

instance Aeson.FromJSON WorkspaceDiagnosticParams where
  parseJSON = Aeson.withObject "WorkspaceDiagnosticParams" $ \arg -> WorkspaceDiagnosticParams <$> arg Aeson..:! "workDoneToken" <*> arg Aeson..:! "partialResultToken" <*> arg Aeson..:! "identifier" <*> arg Aeson..: "previousResultIds"