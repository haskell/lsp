{-# LANGUAGE TemplateHaskell #-}
-- Cyclic dependencies mean we have to put poor StaticRegistrationOptions on its own
module Language.Haskell.LSP.Types.StaticRegistrationOptions where

import Data.Aeson.TH
import Data.Text (Text)
import Language.Haskell.LSP.Types.Constants

data StaticRegistrationOptions =
  StaticRegistrationOptions
    { _id :: Maybe Text
    } deriving (Read,Show,Eq)
deriveJSON lspOptions ''StaticRegistrationOptions
