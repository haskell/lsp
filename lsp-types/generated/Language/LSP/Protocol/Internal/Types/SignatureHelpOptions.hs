-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SignatureHelpOptions where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.Common

{-|
Server Capabilities for a `SignatureHelpRequest`.

-}
data SignatureHelpOptions = SignatureHelpOptions 
  { {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  , {-|
  List of characters that trigger signature help automatically.

  -}
  _triggerCharacters :: (Maybe [Data.Text.Text])
  , {-|
  List of characters that re-trigger signature help.

  These trigger characters are only active when signature help is already showing. All trigger characters
  are also counted as re-trigger characters.

  @since 3.15.0

  -}
  _retriggerCharacters :: (Maybe [Data.Text.Text])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON SignatureHelpOptions where
  toJSON (SignatureHelpOptions arg0 arg1 arg2) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,"triggerCharacters" Language.LSP.Protocol.Types.Common..=? arg1
    ,"retriggerCharacters" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON SignatureHelpOptions where
  parseJSON = Aeson.withObject "SignatureHelpOptions" $ \arg -> SignatureHelpOptions <$> arg Aeson..:! "workDoneProgress" <*> arg Aeson..:! "triggerCharacters" <*> arg Aeson..:! "retriggerCharacters"