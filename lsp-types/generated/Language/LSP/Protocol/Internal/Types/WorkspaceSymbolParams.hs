-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkspaceSymbolParams where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameters of a `WorkspaceSymbolRequest`.

-}
data WorkspaceSymbolParams = WorkspaceSymbolParams 
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
  A query string to filter symbols by. Clients may send an empty
  string here to request all symbols.

  -}
  _query :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON WorkspaceSymbolParams where
  toJSON (WorkspaceSymbolParams arg0 arg1 arg2) = Aeson.object $ concat $  ["workDoneToken" Language.LSP.Protocol.Types.Common..=? arg0
    ,"partialResultToken" Language.LSP.Protocol.Types.Common..=? arg1
    ,["query" Aeson..= arg2]]

instance Aeson.FromJSON WorkspaceSymbolParams where
  parseJSON = Aeson.withObject "WorkspaceSymbolParams" $ \arg -> WorkspaceSymbolParams <$> arg Aeson..:! "workDoneToken" <*> arg Aeson..:! "partialResultToken" <*> arg Aeson..: "query"