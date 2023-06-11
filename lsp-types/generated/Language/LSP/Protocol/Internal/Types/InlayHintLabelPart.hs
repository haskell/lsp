-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlayHintLabelPart where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Command
import qualified Language.LSP.Protocol.Internal.Types.Location
import qualified Language.LSP.Protocol.Internal.Types.MarkupContent
import qualified Language.LSP.Protocol.Types.Common

{-|
An inlay hint label part allows for interactive and composite labels
of inlay hints.

@since 3.17.0
-}
data InlayHintLabelPart = InlayHintLabelPart 
  { {-|
  The value of this label part.
  -}
  _value :: Data.Text.Text
  , {-|
  The tooltip text when you hover over this label part. Depending on
  the client capability `inlayHint.resolveSupport` clients might resolve
  this property late using the resolve request.
  -}
  _tooltip :: (Maybe (Data.Text.Text Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.MarkupContent.MarkupContent))
  , {-|
  An optional source code location that represents this
  label part.

  The editor will use this location for the hover and for code navigation
  features: This part will become a clickable link that resolves to the
  definition of the symbol at the given location (not necessarily the
  location itself), it shows the hover that shows at the given location,
  and it shows a context menu with further code navigation commands.

  Depending on the client capability `inlayHint.resolveSupport` clients
  might resolve this property late using the resolve request.
  -}
  _location :: (Maybe Language.LSP.Protocol.Internal.Types.Location.Location)
  , {-|
  An optional command for this label part.

  Depending on the client capability `inlayHint.resolveSupport` clients
  might resolve this property late using the resolve request.
  -}
  _command :: (Maybe Language.LSP.Protocol.Internal.Types.Command.Command)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON InlayHintLabelPart where
  toJSON (InlayHintLabelPart arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["value" Aeson..= arg0]
    ,"tooltip" Language.LSP.Protocol.Types.Common..=? arg1
    ,"location" Language.LSP.Protocol.Types.Common..=? arg2
    ,"command" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON InlayHintLabelPart where
  parseJSON = Aeson.withObject "InlayHintLabelPart" $ \arg -> InlayHintLabelPart <$> arg Aeson..: "value" <*> arg Aeson..:! "tooltip" <*> arg Aeson..:! "location" <*> arg Aeson..:! "command"
