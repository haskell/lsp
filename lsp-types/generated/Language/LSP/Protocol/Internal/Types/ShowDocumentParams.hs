{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ShowDocumentParams where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
Params to show a resource in the UI.

@since 3.16.0
-}
data ShowDocumentParams = ShowDocumentParams 
  { {-|
  The uri to show.
  -}
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  Indicates to show the resource in an external program.
  To show, for example, `https://code.visualstudio.com/`
  in the default WEB browser set `external` to `true`.
  -}
  _external :: (Maybe Bool)
  , {-|
  An optional property to indicate whether the editor
  showing the document should take focus or not.
  Clients might ignore this property if an external
  program is started.
  -}
  _takeFocus :: (Maybe Bool)
  , {-|
  An optional selection range if the document is a text
  document. Clients might ignore the property if an
  external program is started or the file is not a text
  file.
  -}
  _selection :: (Maybe Language.LSP.Protocol.Internal.Types.Range.Range)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON ShowDocumentParams)

instance Aeson.ToJSON ShowDocumentParams where
  toJSON (ShowDocumentParams arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["uri" Aeson..= arg0]
    ,"external" Language.LSP.Protocol.Types.Common..=? arg1
    ,"takeFocus" Language.LSP.Protocol.Types.Common..=? arg2
    ,"selection" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON ShowDocumentParams where
  parseJSON = Aeson.withObject "ShowDocumentParams" $ \arg -> ShowDocumentParams <$> arg Aeson..: "uri" <*> arg Language.LSP.Protocol.Types.Common..:!? "external" <*> arg Language.LSP.Protocol.Types.Common..:!? "takeFocus" <*> arg Language.LSP.Protocol.Types.Common..:!? "selection"
