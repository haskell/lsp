{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SignatureHelpContext where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.SignatureHelp
import qualified Language.LSP.Protocol.Internal.Types.SignatureHelpTriggerKind
import qualified Language.LSP.Protocol.Types.Common

{-|
Additional information about the context in which a signature help request was triggered.

@since 3.15.0
-}
data SignatureHelpContext = SignatureHelpContext 
  { {-|
  Action that caused signature help to be triggered.
  -}
  _triggerKind :: Language.LSP.Protocol.Internal.Types.SignatureHelpTriggerKind.SignatureHelpTriggerKind
  , {-|
  Character that caused signature help to be triggered.

  This is undefined when `triggerKind !== SignatureHelpTriggerKind.TriggerCharacter`
  -}
  _triggerCharacter :: (Maybe Data.Text.Text)
  , {-|
  `true` if signature help was already showing when it was triggered.

  Retriggers occurs when the signature help is already active and can be caused by actions such as
  typing a trigger character, a cursor move, or document content changes.
  -}
  _isRetrigger :: Bool
  , {-|
  The currently active `SignatureHelp`.

  The `activeSignatureHelp` has its `SignatureHelp.activeSignature` field updated based on
  the user navigating through available signatures.
  -}
  _activeSignatureHelp :: (Maybe Language.LSP.Protocol.Internal.Types.SignatureHelp.SignatureHelp)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SignatureHelpContext)

instance Aeson.ToJSON SignatureHelpContext where
  toJSON (SignatureHelpContext arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["triggerKind" Aeson..= arg0]
    ,"triggerCharacter" Language.LSP.Protocol.Types.Common..=? arg1
    ,["isRetrigger" Aeson..= arg2]
    ,"activeSignatureHelp" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON SignatureHelpContext where
  parseJSON = Aeson.withObject "SignatureHelpContext" $ \arg -> SignatureHelpContext <$> arg Aeson..: "triggerKind" <*> arg Aeson..:! "triggerCharacter" <*> arg Aeson..: "isRetrigger" <*> arg Aeson..:! "activeSignatureHelp"
