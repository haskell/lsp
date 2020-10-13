{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.LSP.Types.TypeDefinition where

import Data.Aeson.TH
import Language.LSP.Types.Progress
import Language.LSP.Types.StaticRegistrationOptions
import Language.LSP.Types.TextDocument
import Language.LSP.Types.Utils

data TypeDefinitionClientCapabilities = TypeDefinitionClientCapabilities
  { -- | Whether implementation supports dynamic registration. If this is set
    -- to 'True'
    -- the client supports the new 'TypeDefinitionRegistrationOptions' return
    -- value for the corresponding server capability as well.
    _dynamicRegistration :: Maybe Bool,
    -- | The client supports additional metadata in the form of definition links.
    --
    -- Since LSP 3.14.0
    _linkSupport :: Maybe Bool
  }
  deriving (Read, Show, Eq)

deriveJSON lspOptions ''TypeDefinitionClientCapabilities

makeExtendingDatatype "TypeDefinitionOptions" [''WorkDoneProgressOptions] []
deriveJSON lspOptions ''TypeDefinitionOptions

makeExtendingDatatype "TypeDefinitionRegistrationOptions"
  [ ''TextDocumentRegistrationOptions
  , ''TypeDefinitionOptions
  , ''StaticRegistrationOptions
  ] []
deriveJSON lspOptions ''TypeDefinitionRegistrationOptions

makeExtendingDatatype "TypeDefinitionParams"
  [ ''TextDocumentPositionParams
  , ''WorkDoneProgressParams
  , ''PartialResultParams
  ] []
deriveJSON lspOptions ''TypeDefinitionParams
