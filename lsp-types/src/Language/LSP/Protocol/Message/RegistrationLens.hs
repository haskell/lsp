{-#LANGUAGE TemplateHaskell #-}

module Language.LSP.Protocol.Message.RegistrationLens where

import Language.LSP.Protocol.Internal.Lens
import Language.LSP.Protocol.Message.Registration
import           Control.Lens.TH

makeFieldsNoPrefix ''TRegistration
makeFieldsNoPrefix ''TUnregistration
