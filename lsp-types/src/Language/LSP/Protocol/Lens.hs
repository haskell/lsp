
module Language.LSP.Protocol.Lens (
  -- * Generated lens classes
    module Lens
  -- Custom lets classes for Message.Types
  , module TypesLens
  -- Custom lens classes for Message.Registration
  , module RegistrationLens
  -- Custom lens classes for Types.SemanticToken
  , module SemanticTokenLens) where

import Language.LSP.Protocol.Internal.Lens as Lens
import Language.LSP.Protocol.Message.TypesLens as TypesLens
import Language.LSP.Protocol.Message.RegistrationLens as RegistrationLens
import Language.LSP.Protocol.Types.SemanticTokensLens as SemanticTokenLens