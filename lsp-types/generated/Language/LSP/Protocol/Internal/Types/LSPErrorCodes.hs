-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.LSPErrorCodes where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Set
import qualified Data.String
import qualified Language.LSP.Protocol.Types.Common
import qualified Language.LSP.Protocol.Types.LspEnum

{-|

-}
data LSPErrorCodes = 
    {-|
  A request failed but it was syntactically correct, e.g the
  method name was known and the parameters were valid. The error
  message should contain human readable information about why
  the request failed.

  @since 3.17.0
  -}
  LSPErrorCodes_RequestFailed
  | {-|
  The server cancelled the request. This error code should
  only be used for requests that explicitly support being
  server cancellable.

  @since 3.17.0
  -}
  LSPErrorCodes_ServerCancelled
  | {-|
  The server detected that the content of a document got
  modified outside normal conditions. A server should
  NOT send this error code if it detects a content change
  in it unprocessed messages. The result even computed
  on an older state might still be useful for the client.

  If a client decides that a result is not of any use anymore
  the client should cancel the request.
  -}
  LSPErrorCodes_ContentModified
  | {-|
  The client has canceled a request and a server as detected
  the cancel.
  -}
  LSPErrorCodes_RequestCancelled
  | LSPErrorCodes_Custom Language.LSP.Protocol.Types.Common.Int32
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)
  deriving ( Aeson.ToJSON
  , Aeson.FromJSON ) via (Language.LSP.Protocol.Types.LspEnum.AsLspEnum LSPErrorCodes Language.LSP.Protocol.Types.Common.Int32)

instance Language.LSP.Protocol.Types.LspEnum.LspEnum LSPErrorCodes where
  knownValues = Data.Set.fromList [LSPErrorCodes_RequestFailed
    ,LSPErrorCodes_ServerCancelled
    ,LSPErrorCodes_ContentModified
    ,LSPErrorCodes_RequestCancelled]
  type EnumBaseType LSPErrorCodes = Language.LSP.Protocol.Types.Common.Int32
  toEnumBaseType LSPErrorCodes_RequestFailed = -32803
  toEnumBaseType LSPErrorCodes_ServerCancelled = -32802
  toEnumBaseType LSPErrorCodes_ContentModified = -32801
  toEnumBaseType LSPErrorCodes_RequestCancelled = -32800
  toEnumBaseType (LSPErrorCodes_Custom arg) = arg

instance Language.LSP.Protocol.Types.LspEnum.LspOpenEnum LSPErrorCodes where
  fromOpenEnumBaseType -32803 = LSPErrorCodes_RequestFailed
  fromOpenEnumBaseType -32802 = LSPErrorCodes_ServerCancelled
  fromOpenEnumBaseType -32801 = LSPErrorCodes_ContentModified
  fromOpenEnumBaseType -32800 = LSPErrorCodes_RequestCancelled
  fromOpenEnumBaseType arg = LSPErrorCodes_Custom arg


