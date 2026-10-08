{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.RenameParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Position
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
The parameters of a `RenameRequest`.
-}
data RenameParams = RenameParams 
  { {-|
  The text document.
  -}
  _textDocument :: Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier
  , {-|
  The position inside the text document.
  -}
  _position :: Language.LSP.Protocol.Internal.Types.Position.Position
  , {-|
  An optional token that a server can use to report work done progress.
  -}
  _workDoneToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|
  The new name of the symbol. If the given name is not valid the
  request must return a `ResponseError` with an
  appropriate message set.
  -}
  _newName :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON RenameParams)

instance Aeson.ToJSON RenameParams where
  toJSON (RenameParams arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["textDocument" Aeson..= arg0]
    ,["position" Aeson..= arg1]
    ,"workDoneToken" Language.LSP.Protocol.Types.Common..=? arg2
    ,["newName" Aeson..= arg3]]

instance Aeson.FromJSON RenameParams where
  parseJSON = Aeson.withObject "RenameParams" $ \arg -> RenameParams <$> arg Aeson..: "textDocument" <*> arg Aeson..: "position" <*> arg Language.LSP.Protocol.Types.Common..:!? "workDoneToken" <*> arg Aeson..: "newName"
