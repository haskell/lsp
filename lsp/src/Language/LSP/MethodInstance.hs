{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LSP.MethodInstance where

import JSONRPC.Method qualified as Untyped
import JSONRPC.Typed.Method

import Language.LSP.Protocol.Message qualified as LSP

import Data.Text qualified as T

deriveClosed ''LSP.Method ''LSP.SMethod

type AdaptInitiator :: LSP.Initiator -> Initiator
type family AdaptInitiator i where
  AdaptInitiator LSP.ServerInitiates = ServerInitiates
  AdaptInitiator LSP.ClientInitiates = ClientInitiates
  AdaptInitiator LSP.EitherInitiates = EitherInitiates

type AdaptType :: LSP.RequestOrNotification -> RequestOrNotification
type family AdaptType t where
  AdaptType LSP.Request = Request
  AdaptType LSP.Notification = Notification

instance Method LSP.Method where
  fromUntypedMethod (Untyped.Method txt) = LSP.methodStringToMethod $ T.unpack txt
  toUntypedMethod = Untyped.Method . T.pack <$> LSP.methodToMethodString

  type MethodInitiator m = AdaptInitiator (LSP.MethodDirection m)
  sMethodInitiator = undefined

  type MethodType m = AdaptType (LSP.MethodType m)
  sMethodType = undefined

  type MethodParams m = LSP.MessageParams m
  type MethodResult m = LSP.MessageResult m
  type ErrorData m = LSP.ErrorData m
