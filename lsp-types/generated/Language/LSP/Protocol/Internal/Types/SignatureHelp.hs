{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SignatureHelp where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.SignatureInformation
import qualified Language.LSP.Protocol.Types.Common

{-|
Signature help represents the signature of something
callable. There can be multiple signature but only one
active and only one active parameter.
-}
data SignatureHelp = SignatureHelp 
  { {-|
  One or more signatures.
  -}
  signatures :: [Language.LSP.Protocol.Internal.Types.SignatureInformation.SignatureInformation]
  , {-|
  The active signature. If omitted or the value lies outside the
  range of `signatures` the value defaults to zero or is ignored if
  the `SignatureHelp` has no signatures.

  Whenever possible implementors should make an active decision about
  the active signature and shouldn't rely on a default value.

  In future version of the protocol this property might become
  mandatory to better express this.
  -}
  activeSignature :: (Maybe Language.LSP.Protocol.Types.Common.UInt)
  , {-|
  The active parameter of the active signature.

  If `null`, no parameter of the signature is active (for example a named
  argument that does not match any declared parameters). This is only valid
  if the client specifies the client capability
  `textDocument.signatureHelp.noActiveParameterSupport === true`

  If omitted or the value lies outside the range of
  `signatures[activeSignature].parameters` defaults to 0 if the active
  signature has parameters.

  If the active signature has no parameters it is ignored.

  In future version of the protocol this property might become
  mandatory (but still nullable) to better express the active parameter if
  the active signature does have any.
  -}
  activeParameter :: (Maybe (Language.LSP.Protocol.Types.Common.UInt Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null))
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON SignatureHelp)

instance Aeson.ToJSON SignatureHelp where
  toJSON (SignatureHelp arg0 arg1 arg2) = Aeson.object $ concat $  [["signatures" Aeson..= arg0]
    ,"activeSignature" Language.LSP.Protocol.Types.Common..=? arg1
    ,"activeParameter" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON SignatureHelp where
  parseJSON = Aeson.withObject "SignatureHelp" $ \arg -> SignatureHelp <$> arg Aeson..: "signatures" <*> arg Language.LSP.Protocol.Types.Common..:!? "activeSignature" <*> arg Language.LSP.Protocol.Types.Common..:!? "activeParameter"
