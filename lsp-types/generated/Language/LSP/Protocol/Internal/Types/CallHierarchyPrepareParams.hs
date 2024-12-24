{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CallHierarchyPrepareParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.Position
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameter of a `textDocument/prepareCallHierarchy` request.

@since 3.16.0
-}
data CallHierarchyPrepareParams = CallHierarchyPrepareParams 
  { {-|
  The text document.
  -}
  textDocument :: Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier
  , {-|
  The position inside the text document.
  -}
  position :: Language.LSP.Protocol.Internal.Types.Position.Position
  , {-|
  An optional token that a server can use to report work done progress.
  -}
  workDoneToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CallHierarchyPrepareParams)

instance Aeson.ToJSON CallHierarchyPrepareParams where
  toJSON (CallHierarchyPrepareParams arg0 arg1 arg2) = Aeson.object $ concat $  [["textDocument" Aeson..= arg0]
    ,["position" Aeson..= arg1]
    ,"workDoneToken" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON CallHierarchyPrepareParams where
  parseJSON = Aeson.withObject "CallHierarchyPrepareParams" $ \arg -> CallHierarchyPrepareParams <$> arg Aeson..: "textDocument" <*> arg Aeson..: "position" <*> arg Language.LSP.Protocol.Types.Common..:!? "workDoneToken"
