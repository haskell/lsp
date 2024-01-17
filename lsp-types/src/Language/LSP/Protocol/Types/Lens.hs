{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LSP.Protocol.Types.Lens where

import Language.LSP.Protocol.Internal.Meta
import Language.LSP.Protocol.Types.SemanticTokens
import Language.LSP.Protocol.Utils.Misc

$(genLenses (structNames ++ [''SemanticTokenAbsolute, ''SemanticTokenRelative]))
