{-#LANGUAGE TemplateHaskell #-}

module Language.LSP.Protocol.Message.Lens where

import Language.LSP.Protocol.Internal.Lens
import Language.LSP.Protocol.Message.Registration
import Language.LSP.Protocol.Message.Types
import           Control.Lens.TH

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
