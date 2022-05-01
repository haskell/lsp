{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Language.LSP.Types.CodeAction where

import           Data.String
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Language.LSP.Types.Internal.Generated (CodeActionKind (..))

-- | Convert a hierarchical string (e.g. "foo.bar") into a 'CodeActionKind'.
fromHierarchicalString :: Text -> CodeActionKind
fromHierarchicalString t = case t of
  ""                       -> CodeActionKind_Empty
  "quickfix"               -> CodeActionKind_QuickFix
  "refactor"               -> CodeActionKind_Refactor
  "refactor.extract"       -> CodeActionKind_RefactorExtract
  "refactor.inline"        -> CodeActionKind_RefactorInline
  "refactor.rewrite"       -> CodeActionKind_RefactorRewrite
  "source"                 -> CodeActionKind_Source
  "source.organizeImports" -> CodeActionKind_SourceOrganizeImports
  "source.fixAll"          -> CodeActionKind_SourceFixAll
  s                        -> CodeActionKind_Custom s

-- | Convert a 'CodeActionKind' into a hierarchical string (e.g. "foo.bar").
toHierarchicalString :: CodeActionKind -> Text
toHierarchicalString k = case k of
  CodeActionKind_Empty                 -> ""
  CodeActionKind_QuickFix              -> "quickfix"
  CodeActionKind_Refactor              -> "refactor"
  CodeActionKind_RefactorExtract       -> "refactor.extract"
  CodeActionKind_RefactorInline        -> "refactor.inline"
  CodeActionKind_RefactorRewrite       -> "refactor.rewrite"
  CodeActionKind_Source                -> "source"
  CodeActionKind_SourceOrganizeImports -> "source.organizeImports"
  CodeActionKind_SourceFixAll          -> "source.fixAll"
  (CodeActionKind_Custom s)            -> s

instance IsString CodeActionKind where
  fromString = fromHierarchicalString . T.pack

-- | Does the first 'CodeActionKind' subsume the other one, hierarchically? Reflexive.
codeActionKindSubsumes :: CodeActionKind -> CodeActionKind -> Bool
-- Simple but ugly implementation: prefix on the string representation
codeActionKindSubsumes parent child = toHierarchicalString parent `T.isPrefixOf` toHierarchicalString child

-- TODO: generate this kind of listing for all the 'open' enums.
-- | The 'CodeActionKind's listed in the LSP spec specifically.
specCodeActionKinds :: [CodeActionKind]
specCodeActionKinds = [
  CodeActionKind_QuickFix
  , CodeActionKind_Refactor
  , CodeActionKind_RefactorExtract
  , CodeActionKind_RefactorInline
  , CodeActionKind_RefactorRewrite
  , CodeActionKind_Source
  , CodeActionKind_SourceOrganizeImports
  , CodeActionKind_SourceFixAll
  ]
