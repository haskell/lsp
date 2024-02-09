{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.LanguageKind where

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
Predefined Language kinds
@since 3.18.0
@proposed
-}
data LanguageKind = 
    {-|

  -}
  LanguageKind_ABAP
  | {-|

  -}
  LanguageKind_WindowsBat
  | {-|

  -}
  LanguageKind_BibTeX
  | {-|

  -}
  LanguageKind_Clojure
  | {-|

  -}
  LanguageKind_Coffeescript
  | {-|

  -}
  LanguageKind_C
  | {-|

  -}
  LanguageKind_CPP
  | {-|

  -}
  LanguageKind_CSharp
  | {-|

  -}
  LanguageKind_CSS
  | {-|

  -}
  LanguageKind_Diff
  | {-|

  -}
  LanguageKind_Dart
  | {-|

  -}
  LanguageKind_Dockerfile
  | {-|

  -}
  LanguageKind_Elixir
  | {-|

  -}
  LanguageKind_Erlang
  | {-|

  -}
  LanguageKind_FSharp
  | {-|

  -}
  LanguageKind_GitCommit
  | {-|

  -}
  LanguageKind_GitRebase
  | {-|

  -}
  LanguageKind_Go
  | {-|

  -}
  LanguageKind_Groovy
  | {-|

  -}
  LanguageKind_Handlebars
  | {-|

  -}
  LanguageKind_Haskell
  | {-|

  -}
  LanguageKind_HTML
  | {-|

  -}
  LanguageKind_Ini
  | {-|

  -}
  LanguageKind_Java
  | {-|

  -}
  LanguageKind_JavaScript
  | {-|

  -}
  LanguageKind_JavaScriptReact
  | {-|

  -}
  LanguageKind_JSON
  | {-|

  -}
  LanguageKind_LaTeX
  | {-|

  -}
  LanguageKind_Less
  | {-|

  -}
  LanguageKind_Lua
  | {-|

  -}
  LanguageKind_Makefile
  | {-|

  -}
  LanguageKind_Markdown
  | {-|

  -}
  LanguageKind_ObjectiveC
  | {-|

  -}
  LanguageKind_ObjectiveCPP
  | {-|

  -}
  LanguageKind_Perl
  | {-|

  -}
  LanguageKind_Perl6
  | {-|

  -}
  LanguageKind_PHP
  | {-|

  -}
  LanguageKind_Powershell
  | {-|

  -}
  LanguageKind_Pug
  | {-|

  -}
  LanguageKind_Python
  | {-|

  -}
  LanguageKind_R
  | {-|

  -}
  LanguageKind_Razor
  | {-|

  -}
  LanguageKind_Ruby
  | {-|

  -}
  LanguageKind_Rust
  | {-|

  -}
  LanguageKind_SCSS
  | {-|

  -}
  LanguageKind_SASS
  | {-|

  -}
  LanguageKind_Scala
  | {-|

  -}
  LanguageKind_ShaderLab
  | {-|

  -}
  LanguageKind_ShellScript
  | {-|

  -}
  LanguageKind_SQL
  | {-|

  -}
  LanguageKind_Swift
  | {-|

  -}
  LanguageKind_TypeScript
  | {-|

  -}
  LanguageKind_TypeScriptReact
  | {-|

  -}
  LanguageKind_TeX
  | {-|

  -}
  LanguageKind_VisualBasic
  | {-|

  -}
  LanguageKind_XML
  | {-|

  -}
  LanguageKind_XSL
  | {-|

  -}
  LanguageKind_YAML
  | LanguageKind_Custom Data.Text.Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON
  , Data.String.IsString ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum LanguageKind)
  deriving Pretty via (ViaJSON LanguageKind)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum LanguageKind where
  knownValues = Data.Set.fromList [LanguageKind_ABAP
    ,LanguageKind_WindowsBat
    ,LanguageKind_BibTeX
    ,LanguageKind_Clojure
    ,LanguageKind_Coffeescript
    ,LanguageKind_C
    ,LanguageKind_CPP
    ,LanguageKind_CSharp
    ,LanguageKind_CSS
    ,LanguageKind_Diff
    ,LanguageKind_Dart
    ,LanguageKind_Dockerfile
    ,LanguageKind_Elixir
    ,LanguageKind_Erlang
    ,LanguageKind_FSharp
    ,LanguageKind_GitCommit
    ,LanguageKind_GitRebase
    ,LanguageKind_Go
    ,LanguageKind_Groovy
    ,LanguageKind_Handlebars
    ,LanguageKind_Haskell
    ,LanguageKind_HTML
    ,LanguageKind_Ini
    ,LanguageKind_Java
    ,LanguageKind_JavaScript
    ,LanguageKind_JavaScriptReact
    ,LanguageKind_JSON
    ,LanguageKind_LaTeX
    ,LanguageKind_Less
    ,LanguageKind_Lua
    ,LanguageKind_Makefile
    ,LanguageKind_Markdown
    ,LanguageKind_ObjectiveC
    ,LanguageKind_ObjectiveCPP
    ,LanguageKind_Perl
    ,LanguageKind_Perl6
    ,LanguageKind_PHP
    ,LanguageKind_Powershell
    ,LanguageKind_Pug
    ,LanguageKind_Python
    ,LanguageKind_R
    ,LanguageKind_Razor
    ,LanguageKind_Ruby
    ,LanguageKind_Rust
    ,LanguageKind_SCSS
    ,LanguageKind_SASS
    ,LanguageKind_Scala
    ,LanguageKind_ShaderLab
    ,LanguageKind_ShellScript
    ,LanguageKind_SQL
    ,LanguageKind_Swift
    ,LanguageKind_TypeScript
    ,LanguageKind_TypeScriptReact
    ,LanguageKind_TeX
    ,LanguageKind_VisualBasic
    ,LanguageKind_XML
    ,LanguageKind_XSL
    ,LanguageKind_YAML]
  type EnumBaseType LanguageKind = Data.Text.Text
  toEnumBaseType LanguageKind_ABAP = "abap"
  toEnumBaseType LanguageKind_WindowsBat = "bat"
  toEnumBaseType LanguageKind_BibTeX = "bibtex"
  toEnumBaseType LanguageKind_Clojure = "clojure"
  toEnumBaseType LanguageKind_Coffeescript = "coffeescript"
  toEnumBaseType LanguageKind_C = "c"
  toEnumBaseType LanguageKind_CPP = "cpp"
  toEnumBaseType LanguageKind_CSharp = "csharp"
  toEnumBaseType LanguageKind_CSS = "css"
  toEnumBaseType LanguageKind_Diff = "diff"
  toEnumBaseType LanguageKind_Dart = "dart"
  toEnumBaseType LanguageKind_Dockerfile = "dockerfile"
  toEnumBaseType LanguageKind_Elixir = "elixir"
  toEnumBaseType LanguageKind_Erlang = "erlang"
  toEnumBaseType LanguageKind_FSharp = "fsharp"
  toEnumBaseType LanguageKind_GitCommit = "git-commit"
  toEnumBaseType LanguageKind_GitRebase = "rebase"
  toEnumBaseType LanguageKind_Go = "go"
  toEnumBaseType LanguageKind_Groovy = "groovy"
  toEnumBaseType LanguageKind_Handlebars = "handlebars"
  toEnumBaseType LanguageKind_Haskell = "haskell"
  toEnumBaseType LanguageKind_HTML = "html"
  toEnumBaseType LanguageKind_Ini = "ini"
  toEnumBaseType LanguageKind_Java = "java"
  toEnumBaseType LanguageKind_JavaScript = "javascript"
  toEnumBaseType LanguageKind_JavaScriptReact = "javascriptreact"
  toEnumBaseType LanguageKind_JSON = "json"
  toEnumBaseType LanguageKind_LaTeX = "latex"
  toEnumBaseType LanguageKind_Less = "less"
  toEnumBaseType LanguageKind_Lua = "lua"
  toEnumBaseType LanguageKind_Makefile = "makefile"
  toEnumBaseType LanguageKind_Markdown = "markdown"
  toEnumBaseType LanguageKind_ObjectiveC = "objective-c"
  toEnumBaseType LanguageKind_ObjectiveCPP = "objective-cpp"
  toEnumBaseType LanguageKind_Perl = "perl"
  toEnumBaseType LanguageKind_Perl6 = "perl6"
  toEnumBaseType LanguageKind_PHP = "php"
  toEnumBaseType LanguageKind_Powershell = "powershell"
  toEnumBaseType LanguageKind_Pug = "jade"
  toEnumBaseType LanguageKind_Python = "python"
  toEnumBaseType LanguageKind_R = "r"
  toEnumBaseType LanguageKind_Razor = "razor"
  toEnumBaseType LanguageKind_Ruby = "ruby"
  toEnumBaseType LanguageKind_Rust = "rust"
  toEnumBaseType LanguageKind_SCSS = "scss"
  toEnumBaseType LanguageKind_SASS = "sass"
  toEnumBaseType LanguageKind_Scala = "scala"
  toEnumBaseType LanguageKind_ShaderLab = "shaderlab"
  toEnumBaseType LanguageKind_ShellScript = "shellscript"
  toEnumBaseType LanguageKind_SQL = "sql"
  toEnumBaseType LanguageKind_Swift = "swift"
  toEnumBaseType LanguageKind_TypeScript = "typescript"
  toEnumBaseType LanguageKind_TypeScriptReact = "typescriptreact"
  toEnumBaseType LanguageKind_TeX = "tex"
  toEnumBaseType LanguageKind_VisualBasic = "vb"
  toEnumBaseType LanguageKind_XML = "xml"
  toEnumBaseType LanguageKind_XSL = "xsl"
  toEnumBaseType LanguageKind_YAML = "yaml"
  toEnumBaseType (LanguageKind_Custom arg) = arg

instance Language.LSP.Protocol.Types.LspEnum.LspOpenEnum LanguageKind where
  fromOpenEnumBaseType "abap" = LanguageKind_ABAP
  fromOpenEnumBaseType "bat" = LanguageKind_WindowsBat
  fromOpenEnumBaseType "bibtex" = LanguageKind_BibTeX
  fromOpenEnumBaseType "clojure" = LanguageKind_Clojure
  fromOpenEnumBaseType "coffeescript" = LanguageKind_Coffeescript
  fromOpenEnumBaseType "c" = LanguageKind_C
  fromOpenEnumBaseType "cpp" = LanguageKind_CPP
  fromOpenEnumBaseType "csharp" = LanguageKind_CSharp
  fromOpenEnumBaseType "css" = LanguageKind_CSS
  fromOpenEnumBaseType "diff" = LanguageKind_Diff
  fromOpenEnumBaseType "dart" = LanguageKind_Dart
  fromOpenEnumBaseType "dockerfile" = LanguageKind_Dockerfile
  fromOpenEnumBaseType "elixir" = LanguageKind_Elixir
  fromOpenEnumBaseType "erlang" = LanguageKind_Erlang
  fromOpenEnumBaseType "fsharp" = LanguageKind_FSharp
  fromOpenEnumBaseType "git-commit" = LanguageKind_GitCommit
  fromOpenEnumBaseType "rebase" = LanguageKind_GitRebase
  fromOpenEnumBaseType "go" = LanguageKind_Go
  fromOpenEnumBaseType "groovy" = LanguageKind_Groovy
  fromOpenEnumBaseType "handlebars" = LanguageKind_Handlebars
  fromOpenEnumBaseType "haskell" = LanguageKind_Haskell
  fromOpenEnumBaseType "html" = LanguageKind_HTML
  fromOpenEnumBaseType "ini" = LanguageKind_Ini
  fromOpenEnumBaseType "java" = LanguageKind_Java
  fromOpenEnumBaseType "javascript" = LanguageKind_JavaScript
  fromOpenEnumBaseType "javascriptreact" = LanguageKind_JavaScriptReact
  fromOpenEnumBaseType "json" = LanguageKind_JSON
  fromOpenEnumBaseType "latex" = LanguageKind_LaTeX
  fromOpenEnumBaseType "less" = LanguageKind_Less
  fromOpenEnumBaseType "lua" = LanguageKind_Lua
  fromOpenEnumBaseType "makefile" = LanguageKind_Makefile
  fromOpenEnumBaseType "markdown" = LanguageKind_Markdown
  fromOpenEnumBaseType "objective-c" = LanguageKind_ObjectiveC
  fromOpenEnumBaseType "objective-cpp" = LanguageKind_ObjectiveCPP
  fromOpenEnumBaseType "perl" = LanguageKind_Perl
  fromOpenEnumBaseType "perl6" = LanguageKind_Perl6
  fromOpenEnumBaseType "php" = LanguageKind_PHP
  fromOpenEnumBaseType "powershell" = LanguageKind_Powershell
  fromOpenEnumBaseType "jade" = LanguageKind_Pug
  fromOpenEnumBaseType "python" = LanguageKind_Python
  fromOpenEnumBaseType "r" = LanguageKind_R
  fromOpenEnumBaseType "razor" = LanguageKind_Razor
  fromOpenEnumBaseType "ruby" = LanguageKind_Ruby
  fromOpenEnumBaseType "rust" = LanguageKind_Rust
  fromOpenEnumBaseType "scss" = LanguageKind_SCSS
  fromOpenEnumBaseType "sass" = LanguageKind_SASS
  fromOpenEnumBaseType "scala" = LanguageKind_Scala
  fromOpenEnumBaseType "shaderlab" = LanguageKind_ShaderLab
  fromOpenEnumBaseType "shellscript" = LanguageKind_ShellScript
  fromOpenEnumBaseType "sql" = LanguageKind_SQL
  fromOpenEnumBaseType "swift" = LanguageKind_Swift
  fromOpenEnumBaseType "typescript" = LanguageKind_TypeScript
  fromOpenEnumBaseType "typescriptreact" = LanguageKind_TypeScriptReact
  fromOpenEnumBaseType "tex" = LanguageKind_TeX
  fromOpenEnumBaseType "vb" = LanguageKind_VisualBasic
  fromOpenEnumBaseType "xml" = LanguageKind_XML
  fromOpenEnumBaseType "xsl" = LanguageKind_XSL
  fromOpenEnumBaseType "yaml" = LanguageKind_YAML
  fromOpenEnumBaseType arg = LanguageKind_Custom arg


