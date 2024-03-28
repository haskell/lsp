{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentSymbolRegistrationOptions where

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
Registration options for a `DocumentSymbolRequest`.
-}
data DocumentSymbolRegistrationOptions = DocumentSymbolRegistrationOptions 
  { {-|
  A document selector to identify the scope of the registration. If set to null
  the document selector provided on the client side will be used.
  -}
  documentSelector :: (Language.LSP.Protocol.Internal.Types.DocumentSelector.DocumentSelector Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|

  -}
  workDoneProgress :: (Maybe Bool)
  , {-|
  A human-readable string that is shown when multiple outlines trees
  are shown for the same document.

  @since 3.16.0
  -}
  label :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DocumentSymbolRegistrationOptions)

instance Aeson.ToJSON DocumentSymbolRegistrationOptions where
  toJSON (DocumentSymbolRegistrationOptions arg0 arg1 arg2) = Aeson.object $ concat $  [["documentSelector" Aeson..= arg0]
    ,"workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg1
    ,"label" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON DocumentSymbolRegistrationOptions where
  parseJSON = Aeson.withObject "DocumentSymbolRegistrationOptions" $ \arg -> DocumentSymbolRegistrationOptions <$> arg Aeson..: "documentSelector" <*> arg Language.LSP.Protocol.Types.Common..:!? "workDoneProgress" <*> arg Language.LSP.Protocol.Types.Common..:!? "label"
