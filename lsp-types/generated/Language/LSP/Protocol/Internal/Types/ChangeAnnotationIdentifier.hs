-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ChangeAnnotationIdentifier where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text

{-|
An identifier to refer to a change annotation stored with a workspace edit.
-}
newtype ChangeAnnotationIdentifier = ChangeAnnotationIdentifier Data.Text.Text
  deriving newtype ( Aeson.ToJSON
  , Aeson.FromJSON
  , Aeson.ToJSONKey
  , Aeson.FromJSONKey )
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ChangeAnnotationIdentifier)
