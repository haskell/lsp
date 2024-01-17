{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LSP.Protocol.Message.Lens where

import Control.Lens.TH
import Language.LSP.Protocol.Message.Registration
import Language.LSP.Protocol.Message.Types
import Language.LSP.Protocol.Types.Lens

makeFieldsNoPrefix ''TRegistration
makeFieldsNoPrefix ''TUnregistration
makeFieldsNoPrefix ''RequestMessage
makeFieldsNoPrefix ''ResponseMessage
makeFieldsNoPrefix ''NotificationMessage
makeFieldsNoPrefix ''ResponseError
makeFieldsNoPrefix ''TRequestMessage
makeFieldsNoPrefix ''TResponseMessage
makeFieldsNoPrefix ''TNotificationMessage
makeFieldsNoPrefix ''TResponseError
