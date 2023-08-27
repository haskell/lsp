module Language.LSP.Protocol.Types.CodeAction where

import Data.Text qualified as T
import Language.LSP.Protocol.Internal.Types
import Language.LSP.Protocol.Types.LspEnum

-- | Does the first 'CodeActionKind' subsume the other one, hierarchically. Reflexive.
codeActionKindSubsumes :: CodeActionKind -> CodeActionKind -> Bool
-- Simple but ugly implementation: prefix on the string representation
codeActionKindSubsumes parent child = toEnumBaseType parent `T.isPrefixOf` toEnumBaseType child
