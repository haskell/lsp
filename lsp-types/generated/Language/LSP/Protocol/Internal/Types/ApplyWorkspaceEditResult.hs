-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ApplyWorkspaceEditResult where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
The result returned from the apply workspace edit request.

@since 3.17 renamed from ApplyWorkspaceEditResponse
-}
data ApplyWorkspaceEditResult = ApplyWorkspaceEditResult 
  { {-|
  Indicates whether the edit was applied or not.
  -}
  _applied :: Bool
  , {-|
  An optional textual description for why the edit was not applied.
  This may be used by the server for diagnostic logging or to provide
  a suitable error for a request that triggered the edit.
  -}
  _failureReason :: (Maybe Data.Text.Text)
  , {-|
  Depending on the client's failure handling strategy `failedChange` might
  contain the index of the change that failed. This property is only available
  if the client signals a `failureHandlingStrategy` in its client capabilities.
  -}
  _failedChange :: (Maybe Language.LSP.Protocol.Types.Common.UInt)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON ApplyWorkspaceEditResult where
  toJSON (ApplyWorkspaceEditResult arg0 arg1 arg2) = Aeson.object $ concat $  [["applied" Aeson..= arg0]
    ,"failureReason" Language.LSP.Protocol.Types.Common..=? arg1
    ,"failedChange" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON ApplyWorkspaceEditResult where
  parseJSON = Aeson.withObject "ApplyWorkspaceEditResult" $ \arg -> ApplyWorkspaceEditResult <$> arg Aeson..: "applied" <*> arg Aeson..:! "failureReason" <*> arg Aeson..:! "failedChange"