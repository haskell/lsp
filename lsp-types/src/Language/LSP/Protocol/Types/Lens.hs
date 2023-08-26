{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LSP.Protocol.Types.Lens where

import Control.Lens.TH
import Language.LSP.Protocol.Internal.Lens
import Language.LSP.Protocol.Types.SemanticTokens

makeFieldsNoPrefix ''SemanticTokenAbsolute
makeFieldsNoPrefix ''SemanticTokenRelative
