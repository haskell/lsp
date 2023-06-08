-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SemanticTokenModifiers where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
A set of predefined token modifiers. This set is not fixed
an clients can specify additional token types via the
corresponding client capabilities.

@since 3.16.0
-}
data SemanticTokenModifiers = 
    {-|

  -}
  SemanticTokenModifiers_Declaration
  | {-|

  -}
  SemanticTokenModifiers_Definition
  | {-|

  -}
  SemanticTokenModifiers_Readonly
  | {-|

  -}
  SemanticTokenModifiers_Static
  | {-|

  -}
  SemanticTokenModifiers_Deprecated
  | {-|

  -}
  SemanticTokenModifiers_Abstract
  | {-|

  -}
  SemanticTokenModifiers_Async
  | {-|

  -}
  SemanticTokenModifiers_Modification
  | {-|

  -}
  SemanticTokenModifiers_Documentation
  | {-|

  -}
  SemanticTokenModifiers_DefaultLibrary
  | SemanticTokenModifiers_Custom Data.Text.Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON
  , Data.String.IsString ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum SemanticTokenModifiers Data.Text.Text)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum SemanticTokenModifiers where
  knownValues = Data.Set.fromList [SemanticTokenModifiers_Declaration
    ,SemanticTokenModifiers_Definition
    ,SemanticTokenModifiers_Readonly
    ,SemanticTokenModifiers_Static
    ,SemanticTokenModifiers_Deprecated
    ,SemanticTokenModifiers_Abstract
    ,SemanticTokenModifiers_Async
    ,SemanticTokenModifiers_Modification
    ,SemanticTokenModifiers_Documentation
    ,SemanticTokenModifiers_DefaultLibrary]
  type EnumBaseType SemanticTokenModifiers = Data.Text.Text
  toEnumBaseType SemanticTokenModifiers_Declaration = "declaration"
  toEnumBaseType SemanticTokenModifiers_Definition = "definition"
  toEnumBaseType SemanticTokenModifiers_Readonly = "readonly"
  toEnumBaseType SemanticTokenModifiers_Static = "static"
  toEnumBaseType SemanticTokenModifiers_Deprecated = "deprecated"
  toEnumBaseType SemanticTokenModifiers_Abstract = "abstract"
  toEnumBaseType SemanticTokenModifiers_Async = "async"
  toEnumBaseType SemanticTokenModifiers_Modification = "modification"
  toEnumBaseType SemanticTokenModifiers_Documentation = "documentation"
  toEnumBaseType SemanticTokenModifiers_DefaultLibrary = "defaultLibrary"
  toEnumBaseType (SemanticTokenModifiers_Custom arg) = arg

instance Language.LSP.Protocol.Types.LspEnum.LspOpenEnum SemanticTokenModifiers where
  fromOpenEnumBaseType "declaration" = SemanticTokenModifiers_Declaration
  fromOpenEnumBaseType "definition" = SemanticTokenModifiers_Definition
  fromOpenEnumBaseType "readonly" = SemanticTokenModifiers_Readonly
  fromOpenEnumBaseType "static" = SemanticTokenModifiers_Static
  fromOpenEnumBaseType "deprecated" = SemanticTokenModifiers_Deprecated
  fromOpenEnumBaseType "abstract" = SemanticTokenModifiers_Abstract
  fromOpenEnumBaseType "async" = SemanticTokenModifiers_Async
  fromOpenEnumBaseType "modification" = SemanticTokenModifiers_Modification
  fromOpenEnumBaseType "documentation" = SemanticTokenModifiers_Documentation
  fromOpenEnumBaseType "defaultLibrary" = SemanticTokenModifiers_DefaultLibrary
  fromOpenEnumBaseType arg = SemanticTokenModifiers_Custom arg


