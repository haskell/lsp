-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkDoneProgressEnd where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Singletons

{-|

-}
data WorkDoneProgressEnd = WorkDoneProgressEnd 
  { {-|

  -}
  _kind :: (Language.LSP.Protocol.Types.Singletons.AString "end")
  , {-|
  Optional, a final message indicating to for example indicate the outcome
  of the operation.
  -}
  _message :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON WorkDoneProgressEnd)

instance Aeson.ToJSON WorkDoneProgressEnd where
  toJSON (WorkDoneProgressEnd arg0 arg1) = Aeson.object $ concat $  [["kind" Aeson..= arg0]
    ,"message" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON WorkDoneProgressEnd where
  parseJSON = Aeson.withObject "WorkDoneProgressEnd" $ \arg -> WorkDoneProgressEnd <$> arg Aeson..: "kind" <*> arg Aeson..:! "message"
