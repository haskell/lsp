-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ShowDocumentParams where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.Uri

{-|
Params to show a document.

@since 3.16.0

-}
data ShowDocumentParams = ShowDocumentParams 
  { {-|
  The document uri to show.

  -}
  _uri :: Language.LSP.Protocol.Types.Uri.Uri
  , {-|
  Indicates to show the resource in an external program.
  To show for example `https://code.visualstudio.com/`
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

instance Aeson.ToJSON ShowDocumentParams where
  toJSON (ShowDocumentParams arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["uri" Aeson..= arg0]
    ,"external" Language.LSP.Protocol.Types.Common..=? arg1
    ,"takeFocus" Language.LSP.Protocol.Types.Common..=? arg2
    ,"selection" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON ShowDocumentParams where
  parseJSON = Aeson.withObject "ShowDocumentParams" $ \arg -> ShowDocumentParams <$> arg Aeson..: "uri" <*> arg Aeson..:! "external" <*> arg Aeson..:! "takeFocus" <*> arg Aeson..:! "selection"