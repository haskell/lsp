-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ParameterInformation where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.MarkupContent
import qualified Language.LSP.Protocol.Types.Common

{-|
Represents a parameter of a callable-signature. A parameter can
have a label and a doc-comment.

-}
data ParameterInformation = ParameterInformation 
  { {-|
  The label of this parameter information.

  Either a string or an inclusive start and exclusive end offsets within its containing
  signature label. (see SignatureInformation.label). The offsets are based on a UTF-16
  string representation as `Position` and `Range` does.

  *Note*: a label of type string should be a substring of its containing signature label.
  Its intended use case is to highlight the parameter label part in the `SignatureInformation.label`.

  -}
  _label :: (Data.Text.Text Language.LSP.Protocol.Types.Common.|? ( Language.LSP.Protocol.Types.Common.UInt
  , Language.LSP.Protocol.Types.Common.UInt ))
  , {-|
  The human-readable doc-comment of this parameter. Will be shown
  in the UI but can be omitted.

  -}
  _documentation :: (Maybe (Data.Text.Text Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.MarkupContent.MarkupContent))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON ParameterInformation where
  toJSON (ParameterInformation arg0 arg1) = Aeson.object $ concat $  [["label" Aeson..= arg0]
    ,"documentation" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON ParameterInformation where
  parseJSON = Aeson.withObject "ParameterInformation" $ \arg -> ParameterInformation <$> arg Aeson..: "label" <*> arg Aeson..:! "documentation"