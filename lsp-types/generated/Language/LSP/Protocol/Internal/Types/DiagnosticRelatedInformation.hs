{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DiagnosticRelatedInformation where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Location
import qualified Language.LSP.Protocol.Types.Common

{-|
Represents a related message and source code location for a diagnostic. This should be
used to point to code locations that cause or related to a diagnostics, e.g when duplicating
a symbol in a scope.
-}
data DiagnosticRelatedInformation = DiagnosticRelatedInformation 
  { {-|
  The location of this related diagnostic information.
  -}
  location :: Language.LSP.Protocol.Internal.Types.Location.Location
  , {-|
  The message of this related diagnostic information.
  -}
  message :: Data.Text.Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DiagnosticRelatedInformation)

instance Aeson.ToJSON DiagnosticRelatedInformation where
  toJSON (DiagnosticRelatedInformation arg0 arg1) = Aeson.object $ concat $  [["location" Aeson..= arg0]
    ,["message" Aeson..= arg1]]

instance Aeson.FromJSON DiagnosticRelatedInformation where
  parseJSON = Aeson.withObject "DiagnosticRelatedInformation" $ \arg -> DiagnosticRelatedInformation <$> arg Aeson..: "location" <*> arg Aeson..: "message"
