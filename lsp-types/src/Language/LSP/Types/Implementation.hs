{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.LSP.Types.Implementation where

import Data.Aeson.TH
import Language.LSP.Types.Progress
import Language.LSP.Types.StaticRegistrationOptions
import Language.LSP.Types.TextDocument
import Language.LSP.Types.Utils

data ImplementationClientCapabilities = ImplementationClientCapabilities
  { -- | Whether implementation supports dynamic registration. If this is set
    -- to 'True'
    -- the client supports the new 'ImplementationRegistrationOptions' return
    -- value for the corresponding server capability as well.
    _dynamicRegistration :: Maybe Bool,
    -- | The client supports additional metadata in the form of definition links.
    --
    -- Since LSP 3.14.0
    _linkSupport :: Maybe Bool
  }
  deriving (Read, Show, Eq)

deriveJSON lspOptions ''ImplementationClientCapabilities

makeExtendingDatatype "ImplementationOptions" [''WorkDoneProgressOptions] []
deriveJSON lspOptions ''ImplementationOptions

makeExtendingDatatype "ImplementationRegistrationOptions"
  [ ''TextDocumentRegistrationOptions
  , ''ImplementationOptions
  , ''StaticRegistrationOptions
  ] []
deriveJSON lspOptions ''ImplementationRegistrationOptions

makeExtendingDatatype "ImplementationParams"
  [ ''TextDocumentPositionParams
  , ''WorkDoneProgressParams
  , ''PartialResultParams
  ] []
deriveJSON lspOptions ''ImplementationParams
