{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentOnTypeFormattingRegistrationOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.DocumentSelector
import qualified Language.LSP.Protocol.Types.Common

{-|
Registration options for a `DocumentOnTypeFormattingRequest`.
-}
data DocumentOnTypeFormattingRegistrationOptions = DocumentOnTypeFormattingRegistrationOptions 
  { {-|
  A document selector to identify the scope of the registration. If set to null
  the document selector provided on the client side will be used.
  -}
  documentSelector :: (Language.LSP.Protocol.Internal.Types.DocumentSelector.DocumentSelector Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|
  A character on which formatting should be triggered, like `{`.
  -}
  firstTriggerCharacter :: Data.Text.Text
  , {-|
  More trigger characters.
  -}
  moreTriggerCharacter :: (Maybe [Data.Text.Text])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DocumentOnTypeFormattingRegistrationOptions)

instance Aeson.ToJSON DocumentOnTypeFormattingRegistrationOptions where
  toJSON (DocumentOnTypeFormattingRegistrationOptions arg0 arg1 arg2) = Aeson.object $ concat $  [["documentSelector" Aeson..= arg0]
    ,["firstTriggerCharacter" Aeson..= arg1]
    ,"moreTriggerCharacter" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON DocumentOnTypeFormattingRegistrationOptions where
  parseJSON = Aeson.withObject "DocumentOnTypeFormattingRegistrationOptions" $ \arg -> DocumentOnTypeFormattingRegistrationOptions <$> arg Aeson..: "documentSelector" <*> arg Aeson..: "firstTriggerCharacter" <*> arg Language.LSP.Protocol.Types.Common..:!? "moreTriggerCharacter"
