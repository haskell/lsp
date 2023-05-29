-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SemanticTokenTypes where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Data.Text
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
A set of predefined token types. This set is not fixed
an clients can specify additional token types via the
corresponding client capabilities.

@since 3.16.0
-}
data SemanticTokenTypes = 
    {-|

  -}
  SemanticTokenTypes_Namespace
  | {-|
  Represents a generic type. Acts as a fallback for types which can't be mapped to
  a specific type like class or enum.
  -}
  SemanticTokenTypes_Type
  | {-|

  -}
  SemanticTokenTypes_Class
  | {-|

  -}
  SemanticTokenTypes_Enum
  | {-|

  -}
  SemanticTokenTypes_Interface
  | {-|

  -}
  SemanticTokenTypes_Struct
  | {-|

  -}
  SemanticTokenTypes_TypeParameter
  | {-|

  -}
  SemanticTokenTypes_Parameter
  | {-|

  -}
  SemanticTokenTypes_Variable
  | {-|

  -}
  SemanticTokenTypes_Property
  | {-|

  -}
  SemanticTokenTypes_EnumMember
  | {-|

  -}
  SemanticTokenTypes_Event
  | {-|

  -}
  SemanticTokenTypes_Function
  | {-|

  -}
  SemanticTokenTypes_Method
  | {-|

  -}
  SemanticTokenTypes_Macro
  | {-|

  -}
  SemanticTokenTypes_Keyword
  | {-|

  -}
  SemanticTokenTypes_Modifier
  | {-|

  -}
  SemanticTokenTypes_Comment
  | {-|

  -}
  SemanticTokenTypes_String
  | {-|

  -}
  SemanticTokenTypes_Number
  | {-|

  -}
  SemanticTokenTypes_Regexp
  | {-|

  -}
  SemanticTokenTypes_Operator
  | {-|
  @since 3.17.0
  -}
  SemanticTokenTypes_Decorator
  | SemanticTokenTypes_Custom Data.Text.Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON
  , Data.String.IsString ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum SemanticTokenTypes Data.Text.Text)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum SemanticTokenTypes where
  knownValues = Data.Set.fromList [SemanticTokenTypes_Namespace
    ,SemanticTokenTypes_Type
    ,SemanticTokenTypes_Class
    ,SemanticTokenTypes_Enum
    ,SemanticTokenTypes_Interface
    ,SemanticTokenTypes_Struct
    ,SemanticTokenTypes_TypeParameter
    ,SemanticTokenTypes_Parameter
    ,SemanticTokenTypes_Variable
    ,SemanticTokenTypes_Property
    ,SemanticTokenTypes_EnumMember
    ,SemanticTokenTypes_Event
    ,SemanticTokenTypes_Function
    ,SemanticTokenTypes_Method
    ,SemanticTokenTypes_Macro
    ,SemanticTokenTypes_Keyword
    ,SemanticTokenTypes_Modifier
    ,SemanticTokenTypes_Comment
    ,SemanticTokenTypes_String
    ,SemanticTokenTypes_Number
    ,SemanticTokenTypes_Regexp
    ,SemanticTokenTypes_Operator
    ,SemanticTokenTypes_Decorator]
  type EnumBaseType SemanticTokenTypes = Data.Text.Text
  toEnumBaseType SemanticTokenTypes_Namespace = "namespace"
  toEnumBaseType SemanticTokenTypes_Type = "type"
  toEnumBaseType SemanticTokenTypes_Class = "class"
  toEnumBaseType SemanticTokenTypes_Enum = "enum"
  toEnumBaseType SemanticTokenTypes_Interface = "interface"
  toEnumBaseType SemanticTokenTypes_Struct = "struct"
  toEnumBaseType SemanticTokenTypes_TypeParameter = "typeParameter"
  toEnumBaseType SemanticTokenTypes_Parameter = "parameter"
  toEnumBaseType SemanticTokenTypes_Variable = "variable"
  toEnumBaseType SemanticTokenTypes_Property = "property"
  toEnumBaseType SemanticTokenTypes_EnumMember = "enumMember"
  toEnumBaseType SemanticTokenTypes_Event = "event"
  toEnumBaseType SemanticTokenTypes_Function = "function"
  toEnumBaseType SemanticTokenTypes_Method = "method"
  toEnumBaseType SemanticTokenTypes_Macro = "macro"
  toEnumBaseType SemanticTokenTypes_Keyword = "keyword"
  toEnumBaseType SemanticTokenTypes_Modifier = "modifier"
  toEnumBaseType SemanticTokenTypes_Comment = "comment"
  toEnumBaseType SemanticTokenTypes_String = "string"
  toEnumBaseType SemanticTokenTypes_Number = "number"
  toEnumBaseType SemanticTokenTypes_Regexp = "regexp"
  toEnumBaseType SemanticTokenTypes_Operator = "operator"
  toEnumBaseType SemanticTokenTypes_Decorator = "decorator"
  toEnumBaseType (SemanticTokenTypes_Custom arg) = arg

instance Language.LSP.Protocol.Types.LspEnum.LspOpenEnum SemanticTokenTypes where
  fromOpenEnumBaseType "namespace" = SemanticTokenTypes_Namespace
  fromOpenEnumBaseType "type" = SemanticTokenTypes_Type
  fromOpenEnumBaseType "class" = SemanticTokenTypes_Class
  fromOpenEnumBaseType "enum" = SemanticTokenTypes_Enum
  fromOpenEnumBaseType "interface" = SemanticTokenTypes_Interface
  fromOpenEnumBaseType "struct" = SemanticTokenTypes_Struct
  fromOpenEnumBaseType "typeParameter" = SemanticTokenTypes_TypeParameter
  fromOpenEnumBaseType "parameter" = SemanticTokenTypes_Parameter
  fromOpenEnumBaseType "variable" = SemanticTokenTypes_Variable
  fromOpenEnumBaseType "property" = SemanticTokenTypes_Property
  fromOpenEnumBaseType "enumMember" = SemanticTokenTypes_EnumMember
  fromOpenEnumBaseType "event" = SemanticTokenTypes_Event
  fromOpenEnumBaseType "function" = SemanticTokenTypes_Function
  fromOpenEnumBaseType "method" = SemanticTokenTypes_Method
  fromOpenEnumBaseType "macro" = SemanticTokenTypes_Macro
  fromOpenEnumBaseType "keyword" = SemanticTokenTypes_Keyword
  fromOpenEnumBaseType "modifier" = SemanticTokenTypes_Modifier
  fromOpenEnumBaseType "comment" = SemanticTokenTypes_Comment
  fromOpenEnumBaseType "string" = SemanticTokenTypes_String
  fromOpenEnumBaseType "number" = SemanticTokenTypes_Number
  fromOpenEnumBaseType "regexp" = SemanticTokenTypes_Regexp
  fromOpenEnumBaseType "operator" = SemanticTokenTypes_Operator
  fromOpenEnumBaseType "decorator" = SemanticTokenTypes_Decorator
  fromOpenEnumBaseType arg = SemanticTokenTypes_Custom arg

