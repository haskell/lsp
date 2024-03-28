{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SignatureInformation where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.MarkupContent
import qualified Language.LSP.Protocol.Internal.Types.ParameterInformation
import qualified Language.LSP.Protocol.Types.Common

{-|
Represents the signature of something callable. A signature
can have a label, like a function-name, a doc-comment, and
a set of parameters.
-}
data SignatureInformation = SignatureInformation 
  { {-|
  The label of this signature. Will be shown in
  the UI.
  -}
  label :: Data.Text.Text
  , {-|
  The human-readable doc-comment of this signature. Will be shown
  in the UI but can be omitted.
  -}
  documentation :: (Maybe (Data.Text.Text Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Internal.Types.MarkupContent.MarkupContent))
  , {-|
  The parameters of this signature.
  -}
  parameters :: (Maybe [Language.LSP.Protocol.Internal.Types.ParameterInformation.ParameterInformation])
  , {-|
  The index of the active parameter.

  If `null`, no parameter of the signature is active (for example a named
  argument that does not match any declared parameters). This is only valid
  if the client specifies the client capability
  `textDocument.signatureHelp.noActiveParameterSupport === true`

  If provided (or `null`), this is used in place of
  `SignatureHelp.activeParameter`.

  @since 3.16.0
  -}
  activeParameter :: (Maybe (Language.LSP.Protocol.Types.Common.UInt Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SignatureInformation)

instance Aeson.ToJSON SignatureInformation where
  toJSON (SignatureInformation arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["label" Aeson..= arg0]
    ,"documentation" Language.LSP.Protocol.Types.Common..=? arg1
    ,"parameters" Language.LSP.Protocol.Types.Common..=? arg2
    ,"activeParameter" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON SignatureInformation where
  parseJSON = Aeson.withObject "SignatureInformation" $ \arg -> SignatureInformation <$> arg Aeson..: "label" <*> arg Language.LSP.Protocol.Types.Common..:!? "documentation" <*> arg Language.LSP.Protocol.Types.Common..:!? "parameters" <*> arg Language.LSP.Protocol.Types.Common..:!? "activeParameter"
