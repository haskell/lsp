
module Language.LSP.Protocol.Lens (
  -- * Generated lens classes
    module Lens
  -- Custom lets classes for Type
  , module TypesLens
  -- Custom lens classes for Message
  , module MessageLens) where

import Language.LSP.Protocol.Internal.Lens as Lens
import Language.LSP.Protocol.Message.Lens as MessageLens
import Language.LSP.Protocol.Types.Lens as TypesLens