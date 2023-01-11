-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.ErrorCodes where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.LspEnum

{-|
Predefined error codes.

-}
data ErrorCodes = 
    {-|

  -}
  ErrorCodes_ParseError
  | {-|

  -}
  ErrorCodes_InvalidRequest
  | {-|

  -}
  ErrorCodes_MethodNotFound
  | {-|

  -}
  ErrorCodes_InvalidParams
  | {-|

  -}
  ErrorCodes_InternalError
  | {-|
  Error code indicating that a server received a notification or
  request before the server has received the `initialize` request.

  -}
  ErrorCodes_ServerNotInitialized
  | {-|

  -}
  ErrorCodes_UnknownErrorCode
  | ErrorCodes_Custom Language.LSP.Protocol.Types.Common.Int32
  deriving stock (Show, Eq, Ord, Generic)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum ErrorCodes Language.LSP.Protocol.Types.Common.Int32)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum ErrorCodes where
  knownValues = Data.Set.fromList [ErrorCodes_ParseError
    ,ErrorCodes_InvalidRequest
    ,ErrorCodes_MethodNotFound
    ,ErrorCodes_InvalidParams
    ,ErrorCodes_InternalError
    ,ErrorCodes_ServerNotInitialized
    ,ErrorCodes_UnknownErrorCode]
  type EnumBaseType ErrorCodes = Language.LSP.Protocol.Types.Common.Int32
  toEnumBaseType ErrorCodes_ParseError = -32700
  toEnumBaseType ErrorCodes_InvalidRequest = -32600
  toEnumBaseType ErrorCodes_MethodNotFound = -32601
  toEnumBaseType ErrorCodes_InvalidParams = -32602
  toEnumBaseType ErrorCodes_InternalError = -32603
  toEnumBaseType ErrorCodes_ServerNotInitialized = -32002
  toEnumBaseType ErrorCodes_UnknownErrorCode = -32001
  toEnumBaseType (ErrorCodes_Custom arg) = arg

instance Language.LSP.Protocol.Types.LspEnum.LspOpenEnum ErrorCodes where
  fromOpenEnumBaseType -32700 = ErrorCodes_ParseError
  fromOpenEnumBaseType -32600 = ErrorCodes_InvalidRequest
  fromOpenEnumBaseType -32601 = ErrorCodes_MethodNotFound
  fromOpenEnumBaseType -32602 = ErrorCodes_InvalidParams
  fromOpenEnumBaseType -32603 = ErrorCodes_InternalError
  fromOpenEnumBaseType -32002 = ErrorCodes_ServerNotInitialized
  fromOpenEnumBaseType -32001 = ErrorCodes_UnknownErrorCode
  fromOpenEnumBaseType arg = ErrorCodes_Custom arg

