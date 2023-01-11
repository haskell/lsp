-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingOptions where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
Provider options for a `DocumentOnTypeFormattingRequest`.

-}
data DocumentOnTypeFormattingOptions = DocumentOnTypeFormattingOptions 
  { {-|
  A character on which formatting should be triggered, like `{`.

  -}
  _firstTriggerCharacter :: Data.Text.Text
  , {-|
  More trigger characters.

  -}
  _moreTriggerCharacter :: (Maybe [Data.Text.Text])
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON DocumentOnTypeFormattingOptions where
  toJSON (DocumentOnTypeFormattingOptions arg0 arg1) = Aeson.object $ concat $  [["firstTriggerCharacter" Aeson..= arg0]
    ,"moreTriggerCharacter" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON DocumentOnTypeFormattingOptions where
  parseJSON = Aeson.withObject "DocumentOnTypeFormattingOptions" $ \arg -> DocumentOnTypeFormattingOptions <$> arg Aeson..: "firstTriggerCharacter" <*> arg Aeson..:! "moreTriggerCharacter"