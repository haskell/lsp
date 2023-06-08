-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WorkDoneProgressCreateParams where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data WorkDoneProgressCreateParams = WorkDoneProgressCreateParams 
  { {-|
  The token to be used to report progress.
  -}
  _token :: Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON WorkDoneProgressCreateParams where
  toJSON (WorkDoneProgressCreateParams arg0) = Aeson.object $ concat $  [["token" Aeson..= arg0]]

instance Aeson.FromJSON WorkDoneProgressCreateParams where
  parseJSON = Aeson.withObject "WorkDoneProgressCreateParams" $ \arg -> WorkDoneProgressCreateParams <$> arg Aeson..: "token"
