{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CodeActionKind where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Set
import qualified Data.String
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
A set of predefined code action kinds
-}
data CodeActionKind = 
    {-|
  Empty kind.
  -}
  CodeActionKind_Empty
  | {-|
  Base kind for quickfix actions: 'quickfix'
  -}
  CodeActionKind_QuickFix
  | {-|
  Base kind for refactoring actions: 'refactor'
  -}
  CodeActionKind_Refactor
  | {-|
  Base kind for refactoring extraction actions: 'refactor.extract'

  Example extract actions:

  - Extract method
  - Extract function
  - Extract variable
  - Extract interface from class
  - ...
  -}
  CodeActionKind_RefactorExtract
  | {-|
  Base kind for refactoring inline actions: 'refactor.inline'

  Example inline actions:

  - Inline function
  - Inline variable
  - Inline constant
  - ...
  -}
  CodeActionKind_RefactorInline
  | {-|
  Base kind for refactoring rewrite actions: 'refactor.rewrite'

  Example rewrite actions:

  - Convert JavaScript function to class
  - Add or remove parameter
  - Encapsulate field
  - Make method static
  - Move method to base class
  - ...
  -}
  CodeActionKind_RefactorRewrite
  | {-|
  Base kind for source actions: `source`

  Source code actions apply to the entire file.
  -}
  CodeActionKind_Source
  | {-|
  Base kind for an organize imports source action: `source.organizeImports`
  -}
  CodeActionKind_SourceOrganizeImports
  | {-|
  Base kind for auto-fix source actions: `source.fixAll`.

  Fix all actions automatically fix errors that have a clear fix that do not require user input.
  They should not suppress errors or perform unsafe fixes such as generating new types or classes.

  @since 3.15.0
  -}
  CodeActionKind_SourceFixAll
  | CodeActionKind_Custom Data.Text.Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON
  , Data.String.IsString ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum CodeActionKind)
  deriving Pretty via (ViaJSON CodeActionKind)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum CodeActionKind where
  knownValues = Data.Set.fromList [CodeActionKind_Empty
    ,CodeActionKind_QuickFix
    ,CodeActionKind_Refactor
    ,CodeActionKind_RefactorExtract
    ,CodeActionKind_RefactorInline
    ,CodeActionKind_RefactorRewrite
    ,CodeActionKind_Source
    ,CodeActionKind_SourceOrganizeImports
    ,CodeActionKind_SourceFixAll]
  type EnumBaseType CodeActionKind = Data.Text.Text
  toEnumBaseType CodeActionKind_Empty = ""
  toEnumBaseType CodeActionKind_QuickFix = "quickfix"
  toEnumBaseType CodeActionKind_Refactor = "refactor"
  toEnumBaseType CodeActionKind_RefactorExtract = "refactor.extract"
  toEnumBaseType CodeActionKind_RefactorInline = "refactor.inline"
  toEnumBaseType CodeActionKind_RefactorRewrite = "refactor.rewrite"
  toEnumBaseType CodeActionKind_Source = "source"
  toEnumBaseType CodeActionKind_SourceOrganizeImports = "source.organizeImports"
  toEnumBaseType CodeActionKind_SourceFixAll = "source.fixAll"
  toEnumBaseType (CodeActionKind_Custom arg) = arg

instance Language.LSP.Protocol.Types.LspEnum.LspOpenEnum CodeActionKind where
  fromOpenEnumBaseType "" = CodeActionKind_Empty
  fromOpenEnumBaseType "quickfix" = CodeActionKind_QuickFix
  fromOpenEnumBaseType "refactor" = CodeActionKind_Refactor
  fromOpenEnumBaseType "refactor.extract" = CodeActionKind_RefactorExtract
  fromOpenEnumBaseType "refactor.inline" = CodeActionKind_RefactorInline
  fromOpenEnumBaseType "refactor.rewrite" = CodeActionKind_RefactorRewrite
  fromOpenEnumBaseType "source" = CodeActionKind_Source
  fromOpenEnumBaseType "source.organizeImports" = CodeActionKind_SourceOrganizeImports
  fromOpenEnumBaseType "source.fixAll" = CodeActionKind_SourceFixAll
  fromOpenEnumBaseType arg = CodeActionKind_Custom arg


