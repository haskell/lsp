{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ColorPresentationParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.Color
import qualified Language.LSP.Protocol.Internal.Types.ProgressToken
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier
import qualified Language.LSP.Protocol.Types.Common

{-|
Parameters for a `ColorPresentationRequest`.
-}
data ColorPresentationParams = ColorPresentationParams 
  { {-|
  An optional token that a server can use to report work done progress.
  -}
  workDoneToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|
  An optional token that a server can use to report partial results (e.g. streaming) to
  the client.
  -}
  partialResultToken :: (Maybe Language.LSP.Protocol.Internal.Types.ProgressToken.ProgressToken)
  , {-|
  The text document.
  -}
  textDocument :: Language.LSP.Protocol.Internal.Types.TextDocumentIdentifier.TextDocumentIdentifier
  , {-|
  The color to request presentations for.
  -}
  color :: Language.LSP.Protocol.Internal.Types.Color.Color
  , {-|
  The range where the color would be inserted. Serves as a context.
  -}
  range :: Language.LSP.Protocol.Internal.Types.Range.Range
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ColorPresentationParams)

instance Aeson.ToJSON ColorPresentationParams where
  toJSON (ColorPresentationParams arg0 arg1 arg2 arg3 arg4) = Aeson.object $ concat $  ["workDoneToken" Language.LSP.Protocol.Types.Common..=? arg0
    ,"partialResultToken" Language.LSP.Protocol.Types.Common..=? arg1
    ,["textDocument" Aeson..= arg2]
    ,["color" Aeson..= arg3]
    ,["range" Aeson..= arg4]]

instance Aeson.FromJSON ColorPresentationParams where
  parseJSON = Aeson.withObject "ColorPresentationParams" $ \arg -> ColorPresentationParams <$> arg Language.LSP.Protocol.Types.Common..:!? "workDoneToken" <*> arg Language.LSP.Protocol.Types.Common..:!? "partialResultToken" <*> arg Aeson..: "textDocument" <*> arg Aeson..: "color" <*> arg Aeson..: "range"
