-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SignatureHelp where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
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
  _signatures :: [Language.LSP.Protocol.Internal.Types.SignatureInformation.SignatureInformation]
  , {-|
  The active signature. If omitted or the value lies outside the
  range of `signatures` the value defaults to zero or is ignored if
  the `SignatureHelp` has no signatures.

  Whenever possible implementors should make an active decision about
  the active signature and shouldn't rely on a default value.

  In future version of the protocol this property might become
  mandatory to better express this.
  -}
  _activeSignature :: (Maybe Language.LSP.Protocol.Types.Common.UInt)
  , {-|
  The active parameter of the active signature. If omitted or the value
  lies outside the range of `signatures[activeSignature].parameters`
  defaults to 0 if the active signature has parameters. If
  the active signature has no parameters it is ignored.
  In future version of the protocol this property might become
  mandatory to better express the active parameter if the
  active signature does have any.
  -}
  _activeParameter :: (Maybe Language.LSP.Protocol.Types.Common.UInt)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON SignatureHelp where
  toJSON (SignatureHelp arg0 arg1 arg2) = Aeson.object $ concat $  [["signatures" Aeson..= arg0]
    ,"activeSignature" Language.LSP.Protocol.Types.Common..=? arg1
    ,"activeParameter" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON SignatureHelp where
  parseJSON = Aeson.withObject "SignatureHelp" $ \arg -> SignatureHelp <$> arg Aeson..: "signatures" <*> arg Aeson..:! "activeSignature" <*> arg Aeson..:! "activeParameter"