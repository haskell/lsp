-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.LogTraceParams where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data LogTraceParams = LogTraceParams 
  { {-|

  -}
  _message :: Data.Text.Text
  , {-|

  -}
  _verbose :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON LogTraceParams where
  toJSON (LogTraceParams arg0 arg1) = Aeson.object $ concat $  [["message" Aeson..= arg0]
    ,"verbose" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON LogTraceParams where
  parseJSON = Aeson.withObject "LogTraceParams" $ \arg -> LogTraceParams <$> arg Aeson..: "message" <*> arg Aeson..:! "verbose"
