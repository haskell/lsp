-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.SymbolKind where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
A symbol kind.
-}
data SymbolKind = 
    {-|

  -}
  SymbolKind_File
  | {-|

  -}
  SymbolKind_Module
  | {-|

  -}
  SymbolKind_Namespace
  | {-|

  -}
  SymbolKind_Package
  | {-|

  -}
  SymbolKind_Class
  | {-|

  -}
  SymbolKind_Method
  | {-|

  -}
  SymbolKind_Property
  | {-|

  -}
  SymbolKind_Field
  | {-|

  -}
  SymbolKind_Constructor
  | {-|

  -}
  SymbolKind_Enum
  | {-|

  -}
  SymbolKind_Interface
  | {-|

  -}
  SymbolKind_Function
  | {-|

  -}
  SymbolKind_Variable
  | {-|

  -}
  SymbolKind_Constant
  | {-|

  -}
  SymbolKind_String
  | {-|

  -}
  SymbolKind_Number
  | {-|

  -}
  SymbolKind_Boolean
  | {-|

  -}
  SymbolKind_Array
  | {-|

  -}
  SymbolKind_Object
  | {-|

  -}
  SymbolKind_Key
  | {-|

  -}
  SymbolKind_Null
  | {-|

  -}
  SymbolKind_EnumMember
  | {-|

  -}
  SymbolKind_Struct
  | {-|

  -}
  SymbolKind_Event
  | {-|

  -}
  SymbolKind_Operator
  | {-|

  -}
  SymbolKind_TypeParameter
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum SymbolKind Language.LSP.Protocol.Types.Common.UInt)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum SymbolKind where
  knownValues = Data.Set.fromList [SymbolKind_File
    ,SymbolKind_Module
    ,SymbolKind_Namespace
    ,SymbolKind_Package
    ,SymbolKind_Class
    ,SymbolKind_Method
    ,SymbolKind_Property
    ,SymbolKind_Field
    ,SymbolKind_Constructor
    ,SymbolKind_Enum
    ,SymbolKind_Interface
    ,SymbolKind_Function
    ,SymbolKind_Variable
    ,SymbolKind_Constant
    ,SymbolKind_String
    ,SymbolKind_Number
    ,SymbolKind_Boolean
    ,SymbolKind_Array
    ,SymbolKind_Object
    ,SymbolKind_Key
    ,SymbolKind_Null
    ,SymbolKind_EnumMember
    ,SymbolKind_Struct
    ,SymbolKind_Event
    ,SymbolKind_Operator
    ,SymbolKind_TypeParameter]
  type EnumBaseType SymbolKind = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType SymbolKind_File = 1
  toEnumBaseType SymbolKind_Module = 2
  toEnumBaseType SymbolKind_Namespace = 3
  toEnumBaseType SymbolKind_Package = 4
  toEnumBaseType SymbolKind_Class = 5
  toEnumBaseType SymbolKind_Method = 6
  toEnumBaseType SymbolKind_Property = 7
  toEnumBaseType SymbolKind_Field = 8
  toEnumBaseType SymbolKind_Constructor = 9
  toEnumBaseType SymbolKind_Enum = 10
  toEnumBaseType SymbolKind_Interface = 11
  toEnumBaseType SymbolKind_Function = 12
  toEnumBaseType SymbolKind_Variable = 13
  toEnumBaseType SymbolKind_Constant = 14
  toEnumBaseType SymbolKind_String = 15
  toEnumBaseType SymbolKind_Number = 16
  toEnumBaseType SymbolKind_Boolean = 17
  toEnumBaseType SymbolKind_Array = 18
  toEnumBaseType SymbolKind_Object = 19
  toEnumBaseType SymbolKind_Key = 20
  toEnumBaseType SymbolKind_Null = 21
  toEnumBaseType SymbolKind_EnumMember = 22
  toEnumBaseType SymbolKind_Struct = 23
  toEnumBaseType SymbolKind_Event = 24
  toEnumBaseType SymbolKind_Operator = 25
  toEnumBaseType SymbolKind_TypeParameter = 26
  fromEnumBaseType 1 = pure SymbolKind_File
  fromEnumBaseType 2 = pure SymbolKind_Module
  fromEnumBaseType 3 = pure SymbolKind_Namespace
  fromEnumBaseType 4 = pure SymbolKind_Package
  fromEnumBaseType 5 = pure SymbolKind_Class
  fromEnumBaseType 6 = pure SymbolKind_Method
  fromEnumBaseType 7 = pure SymbolKind_Property
  fromEnumBaseType 8 = pure SymbolKind_Field
  fromEnumBaseType 9 = pure SymbolKind_Constructor
  fromEnumBaseType 10 = pure SymbolKind_Enum
  fromEnumBaseType 11 = pure SymbolKind_Interface
  fromEnumBaseType 12 = pure SymbolKind_Function
  fromEnumBaseType 13 = pure SymbolKind_Variable
  fromEnumBaseType 14 = pure SymbolKind_Constant
  fromEnumBaseType 15 = pure SymbolKind_String
  fromEnumBaseType 16 = pure SymbolKind_Number
  fromEnumBaseType 17 = pure SymbolKind_Boolean
  fromEnumBaseType 18 = pure SymbolKind_Array
  fromEnumBaseType 19 = pure SymbolKind_Object
  fromEnumBaseType 20 = pure SymbolKind_Key
  fromEnumBaseType 21 = pure SymbolKind_Null
  fromEnumBaseType 22 = pure SymbolKind_EnumMember
  fromEnumBaseType 23 = pure SymbolKind_Struct
  fromEnumBaseType 24 = pure SymbolKind_Event
  fromEnumBaseType 25 = pure SymbolKind_Operator
  fromEnumBaseType 26 = pure SymbolKind_TypeParameter
  fromEnumBaseType _ = Nothing

