{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.WindowClientCapabilities where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.ShowDocumentClientCapabilities
import qualified Language.LSP.Protocol.Internal.Types.ShowMessageRequestClientCapabilities
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data WindowClientCapabilities = WindowClientCapabilities 
  { {-|
  It indicates whether the client supports server initiated
  progress using the `window/workDoneProgress/create` request.

  The capability also controls Whether client supports handling
  of progress notifications. If set servers are allowed to report a
  `workDoneProgress` property in the request specific server
  capabilities.

  @since 3.15.0
  -}
  workDoneProgress :: (Maybe Bool)
  , {-|
  Capabilities specific to the showMessage request.

  @since 3.16.0
  -}
  showMessage :: (Maybe Language.LSP.Protocol.Internal.Types.ShowMessageRequestClientCapabilities.ShowMessageRequestClientCapabilities)
  , {-|
  Capabilities specific to the showDocument request.

  @since 3.16.0
  -}
  showDocument :: (Maybe Language.LSP.Protocol.Internal.Types.ShowDocumentClientCapabilities.ShowDocumentClientCapabilities)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON WindowClientCapabilities)

instance Aeson.ToJSON WindowClientCapabilities where
  toJSON (WindowClientCapabilities arg0 arg1 arg2) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,"showMessage" Language.LSP.Protocol.Types.Common..=? arg1
    ,"showDocument" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON WindowClientCapabilities where
  parseJSON = Aeson.withObject "WindowClientCapabilities" $ \arg -> WindowClientCapabilities <$> arg Language.LSP.Protocol.Types.Common..:!? "workDoneProgress" <*> arg Language.LSP.Protocol.Types.Common..:!? "showMessage" <*> arg Language.LSP.Protocol.Types.Common..:!? "showDocument"
