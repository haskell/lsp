module Language.LSP.Protocol.Types.CodeAction where

import           Language.LSP.Protocol.Types.LspEnum
import           Language.LSP.Protocol.Internal.Types
import qualified Data.Text as T

-- | Does the first 'CodeActionKind' subsume the other one, hierarchically. Reflexive.
codeActionKindSubsumes :: CodeActionKind -> CodeActionKind -> Bool
-- Simple but ugly implementation: prefix on the string representation
codeActionKindSubsumes parent child = toEnumBaseType parent `T.isPrefixOf` toEnumBaseType child
