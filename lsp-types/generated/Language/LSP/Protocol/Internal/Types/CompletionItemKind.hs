{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CompletionItemKind where

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
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
The kind of a completion entry.
-}
data CompletionItemKind = 
    {-|

  -}
  CompletionItemKind_Text
  | {-|

  -}
  CompletionItemKind_Method
  | {-|

  -}
  CompletionItemKind_Function
  | {-|

  -}
  CompletionItemKind_Constructor
  | {-|

  -}
  CompletionItemKind_Field
  | {-|

  -}
  CompletionItemKind_Variable
  | {-|

  -}
  CompletionItemKind_Class
  | {-|

  -}
  CompletionItemKind_Interface
  | {-|

  -}
  CompletionItemKind_Module
  | {-|

  -}
  CompletionItemKind_Property
  | {-|

  -}
  CompletionItemKind_Unit
  | {-|

  -}
  CompletionItemKind_Value
  | {-|

  -}
  CompletionItemKind_Enum
  | {-|

  -}
  CompletionItemKind_Keyword
  | {-|

  -}
  CompletionItemKind_Snippet
  | {-|

  -}
  CompletionItemKind_Color
  | {-|

  -}
  CompletionItemKind_File
  | {-|

  -}
  CompletionItemKind_Reference
  | {-|

  -}
  CompletionItemKind_Folder
  | {-|

  -}
  CompletionItemKind_EnumMember
  | {-|

  -}
  CompletionItemKind_Constant
  | {-|

  -}
  CompletionItemKind_Struct
  | {-|

  -}
  CompletionItemKind_Event
  | {-|

  -}
  CompletionItemKind_Operator
  | {-|

  -}
  CompletionItemKind_TypeParameter
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum CompletionItemKind Language.LSP.Protocol.Types.Common.UInt)
  deriving Pretty via (ViaJSON CompletionItemKind)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum CompletionItemKind where
  knownValues = Data.Set.fromList [CompletionItemKind_Text
    ,CompletionItemKind_Method
    ,CompletionItemKind_Function
    ,CompletionItemKind_Constructor
    ,CompletionItemKind_Field
    ,CompletionItemKind_Variable
    ,CompletionItemKind_Class
    ,CompletionItemKind_Interface
    ,CompletionItemKind_Module
    ,CompletionItemKind_Property
    ,CompletionItemKind_Unit
    ,CompletionItemKind_Value
    ,CompletionItemKind_Enum
    ,CompletionItemKind_Keyword
    ,CompletionItemKind_Snippet
    ,CompletionItemKind_Color
    ,CompletionItemKind_File
    ,CompletionItemKind_Reference
    ,CompletionItemKind_Folder
    ,CompletionItemKind_EnumMember
    ,CompletionItemKind_Constant
    ,CompletionItemKind_Struct
    ,CompletionItemKind_Event
    ,CompletionItemKind_Operator
    ,CompletionItemKind_TypeParameter]
  type EnumBaseType CompletionItemKind = Language.LSP.Protocol.Types.Common.UInt
  toEnumBaseType CompletionItemKind_Text = 1
  toEnumBaseType CompletionItemKind_Method = 2
  toEnumBaseType CompletionItemKind_Function = 3
  toEnumBaseType CompletionItemKind_Constructor = 4
  toEnumBaseType CompletionItemKind_Field = 5
  toEnumBaseType CompletionItemKind_Variable = 6
  toEnumBaseType CompletionItemKind_Class = 7
  toEnumBaseType CompletionItemKind_Interface = 8
  toEnumBaseType CompletionItemKind_Module = 9
  toEnumBaseType CompletionItemKind_Property = 10
  toEnumBaseType CompletionItemKind_Unit = 11
  toEnumBaseType CompletionItemKind_Value = 12
  toEnumBaseType CompletionItemKind_Enum = 13
  toEnumBaseType CompletionItemKind_Keyword = 14
  toEnumBaseType CompletionItemKind_Snippet = 15
  toEnumBaseType CompletionItemKind_Color = 16
  toEnumBaseType CompletionItemKind_File = 17
  toEnumBaseType CompletionItemKind_Reference = 18
  toEnumBaseType CompletionItemKind_Folder = 19
  toEnumBaseType CompletionItemKind_EnumMember = 20
  toEnumBaseType CompletionItemKind_Constant = 21
  toEnumBaseType CompletionItemKind_Struct = 22
  toEnumBaseType CompletionItemKind_Event = 23
  toEnumBaseType CompletionItemKind_Operator = 24
  toEnumBaseType CompletionItemKind_TypeParameter = 25
  fromEnumBaseType 1 = pure CompletionItemKind_Text
  fromEnumBaseType 2 = pure CompletionItemKind_Method
  fromEnumBaseType 3 = pure CompletionItemKind_Function
  fromEnumBaseType 4 = pure CompletionItemKind_Constructor
  fromEnumBaseType 5 = pure CompletionItemKind_Field
  fromEnumBaseType 6 = pure CompletionItemKind_Variable
  fromEnumBaseType 7 = pure CompletionItemKind_Class
  fromEnumBaseType 8 = pure CompletionItemKind_Interface
  fromEnumBaseType 9 = pure CompletionItemKind_Module
  fromEnumBaseType 10 = pure CompletionItemKind_Property
  fromEnumBaseType 11 = pure CompletionItemKind_Unit
  fromEnumBaseType 12 = pure CompletionItemKind_Value
  fromEnumBaseType 13 = pure CompletionItemKind_Enum
  fromEnumBaseType 14 = pure CompletionItemKind_Keyword
  fromEnumBaseType 15 = pure CompletionItemKind_Snippet
  fromEnumBaseType 16 = pure CompletionItemKind_Color
  fromEnumBaseType 17 = pure CompletionItemKind_File
  fromEnumBaseType 18 = pure CompletionItemKind_Reference
  fromEnumBaseType 19 = pure CompletionItemKind_Folder
  fromEnumBaseType 20 = pure CompletionItemKind_EnumMember
  fromEnumBaseType 21 = pure CompletionItemKind_Constant
  fromEnumBaseType 22 = pure CompletionItemKind_Struct
  fromEnumBaseType 23 = pure CompletionItemKind_Event
  fromEnumBaseType 24 = pure CompletionItemKind_Operator
  fromEnumBaseType 25 = pure CompletionItemKind_TypeParameter
  fromEnumBaseType _ = Nothing


