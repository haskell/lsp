{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LSP.Protocol.Message.Lens where

import Control.Lens.TH
import Language.LSP.Protocol.Message.Registration
import Language.LSP.Protocol.Types.Lens

makeFieldsNoPrefix ''TRegistration
makeFieldsNoPrefix ''TUnregistration
