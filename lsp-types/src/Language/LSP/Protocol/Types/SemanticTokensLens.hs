{-#LANGUAGE TemplateHaskell #-}

module Language.LSP.Protocol.Types.SemanticTokensLens where

import Language.LSP.Protocol.Internal.Lens
import Language.LSP.Protocol.Types.SemanticTokens
import           Control.Lens.TH

makeFieldsNoPrefix ''SemanticTokenAbsolute
makeFieldsNoPrefix ''SemanticTokenRelative