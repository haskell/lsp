{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SignatureHelpRegistrationOptions where

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
Registration options for a `SignatureHelpRequest`.
-}
data SignatureHelpRegistrationOptions = SignatureHelpRegistrationOptions 
  { {-|
  A document selector to identify the scope of the registration. If set to null
  the document selector provided on the client side will be used.
  -}
  documentSelector :: (Language.LSP.Protocol.Internal.Types.DocumentSelector.DocumentSelector Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|

  -}
  workDoneProgress :: (Maybe Bool)
  , {-|
  List of characters that trigger signature help automatically.
  -}
  triggerCharacters :: (Maybe [Data.Text.Text])
  , {-|
  List of characters that re-trigger signature help.

  These trigger characters are only active when signature help is already showing. All trigger characters
  are also counted as re-trigger characters.

  @since 3.15.0
  -}
  retriggerCharacters :: (Maybe [Data.Text.Text])
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SignatureHelpRegistrationOptions)

instance Aeson.ToJSON SignatureHelpRegistrationOptions where
  toJSON (SignatureHelpRegistrationOptions arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["documentSelector" Aeson..= arg0]
    ,"workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg1
    ,"triggerCharacters" Language.LSP.Protocol.Types.Common..=? arg2
    ,"retriggerCharacters" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON SignatureHelpRegistrationOptions where
  parseJSON = Aeson.withObject "SignatureHelpRegistrationOptions" $ \arg -> SignatureHelpRegistrationOptions <$> arg Aeson..: "documentSelector" <*> arg Language.LSP.Protocol.Types.Common..:!? "workDoneProgress" <*> arg Language.LSP.Protocol.Types.Common..:!? "triggerCharacters" <*> arg Language.LSP.Protocol.Types.Common..:!? "retriggerCharacters"
