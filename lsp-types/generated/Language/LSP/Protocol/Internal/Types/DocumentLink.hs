{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentLink where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
A document link is a range in a text document that links to an internal or external resource, like another
text document or a web site.
-}
data DocumentLink = DocumentLink 
  { {-|
  The range this link applies to.
  -}
  range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The uri this link points to. If missing a resolve request is sent later.
  -}
  target :: (Maybe Language.LSP.Protocol.Types.Uri.Uri)
  , {-|
  The tooltip text when you hover over this link.

  If a tooltip is provided, is will be displayed in a string that includes instructions on how to
  trigger the link, such as `{0} (ctrl + click)`. The specific instructions vary depending on OS,
  user settings, and localization.

  @since 3.15.0
  -}
  tooltip :: (Maybe Data.Text.Text)
  , {-|
  A data entry field that is preserved on a document link between a
  DocumentLinkRequest and a DocumentLinkResolveRequest.
  -}
  data_ :: (Maybe Data.Aeson.Value)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DocumentLink)

instance Aeson.ToJSON DocumentLink where
  toJSON (DocumentLink arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["range" Aeson..= arg0]
    ,"target" Language.LSP.Protocol.Types.Common..=? arg1
    ,"tooltip" Language.LSP.Protocol.Types.Common..=? arg2
    ,"data" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON DocumentLink where
  parseJSON = Aeson.withObject "DocumentLink" $ \arg -> DocumentLink <$> arg Aeson..: "range" <*> arg Language.LSP.Protocol.Types.Common..:!? "target" <*> arg Language.LSP.Protocol.Types.Common..:!? "tooltip" <*> arg Language.LSP.Protocol.Types.Common..:!? "data"
