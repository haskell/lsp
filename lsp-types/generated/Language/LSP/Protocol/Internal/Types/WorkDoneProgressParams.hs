-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkDoneProgressParams where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data WorkDoneProgressParams = WorkDoneProgressParams 
  { {-|
  An optional token that a server can use to report work done progress.
  -}
  _workDoneToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON WorkDoneProgressParams where
  toJSON (WorkDoneProgressParams arg0) = Aeson.object $ concat $  ["workDoneToken" Language.LSP.Protocol.Types.Common..=? arg0]

instance Aeson.FromJSON WorkDoneProgressParams where
  parseJSON = Aeson.withObject "WorkDoneProgressParams" $ \arg -> WorkDoneProgressParams <$> arg Aeson..:! "workDoneToken"