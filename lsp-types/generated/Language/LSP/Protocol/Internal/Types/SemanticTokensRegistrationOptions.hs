-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SemanticTokensRegistrationOptions where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.DocumentSelector
import qualified Language.LSP.Protocol.Internal.Types.SemanticTokensLegend
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.16.0

-}
data SemanticTokensRegistrationOptions = SemanticTokensRegistrationOptions 
  { {-|
  A document selector to identify the scope of the registration. If set to null
  the document selector provided on the client side will be used.

  -}
  _documentSelector :: (Language.LSP.Protocol.Internal.Types.DocumentSelector.DocumentSelector Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  , {-|
  The legend used by the server

  -}
  _legend :: Language.LSP.Protocol.Internal.Types.SemanticTokensLegend.SemanticTokensLegend
  , {-|
  Server supports providing semantic tokens for a specific range
  of a document.

  -}
  _range :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Row.Rec Row.Empty)))
  , {-|
  Server supports providing semantic tokens for a full document.

  -}
  _full :: (Maybe (Bool Language.LSP.Protocol.Types.Common.|? (Row.Rec ("delta" Row..== (Maybe Bool) Row..+ Row.Empty))))
  , {-|
  The id used to register the request. The id can be used to deregister
  the request again. See also Registration#id.

  -}
  _id :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON SemanticTokensRegistrationOptions where
  toJSON (SemanticTokensRegistrationOptions arg0 arg1 arg2 arg3 arg4 arg5) = Aeson.object $ concat $  [["documentSelector" Aeson..= arg0]
    ,"workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg1
    ,["legend" Aeson..= arg2]
    ,"range" Language.LSP.Protocol.Types.Common..=? arg3
    ,"full" Language.LSP.Protocol.Types.Common..=? arg4
    ,"id" Language.LSP.Protocol.Types.Common..=? arg5]

instance Aeson.FromJSON SemanticTokensRegistrationOptions where
  parseJSON = Aeson.withObject "SemanticTokensRegistrationOptions" $ \arg -> SemanticTokensRegistrationOptions <$> arg Aeson..: "documentSelector" <*> arg Aeson..:! "workDoneProgress" <*> arg Aeson..: "legend" <*> arg Aeson..:! "range" <*> arg Aeson..:! "full" <*> arg Aeson..:! "id"