{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.TextDocumentContentChangeEvent where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row as Row
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
An event describing a change to a text document. If only a text is provided
it is considered to be the full content of the document.
-}
newtype TextDocumentContentChangeEvent = TextDocumentContentChangeEvent ((Row.Rec ("range" Row..== Language.LSP.Protocol.Internal.Types.Range.Range Row..+ ("rangeLength" Row..== (Maybe Language.LSP.Protocol.Types.Common.UInt) Row..+ ("text" Row..== Data.Text.Text Row..+ Row.Empty)))) Language.LSP.Protocol.Types.Common.|? (Row.Rec ("text" Row..== Data.Text.Text Row..+ Row.Empty)))
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON TextDocumentContentChangeEvent)
