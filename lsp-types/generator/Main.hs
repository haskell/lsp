{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import CodeGen
import Language.LSP.MetaModel

-- Run this from the lsp-types directory with "cabal run generator" to regenerate
-- the generated files from the metamodel.
main :: IO ()
main = genFromMetaModel "Language.LSP.Protocol.Internal" "generated" metaModel
