-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CompletionContext where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.CompletionTriggerKind
import qualified Language.LSP.Protocol.Types.Common

{-|
Contains additional information about the context in which a completion request is triggered.

-}
data CompletionContext = CompletionContext 
  { {-|
  How the completion was triggered.

  -}
  _triggerKind :: Language.LSP.Protocol.Internal.Types.CompletionTriggerKind.CompletionTriggerKind
  , {-|
  The trigger character (a single character) that has trigger code complete.
  Is undefined if `triggerKind !== CompletionTriggerKind.TriggerCharacter`

  -}
  _triggerCharacter :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON CompletionContext where
  toJSON (CompletionContext arg0 arg1) = Aeson.object $ concat $  [["triggerKind" Aeson..= arg0]
    ,"triggerCharacter" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON CompletionContext where
  parseJSON = Aeson.withObject "CompletionContext" $ \arg -> CompletionContext <$> arg Aeson..: "triggerKind" <*> arg Aeson..:! "triggerCharacter"