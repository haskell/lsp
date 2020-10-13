{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

module Language.LSP.Types.Definition where
    
import Data.Aeson.TH
import Language.LSP.Types.Progress
import Language.LSP.Types.TextDocument
import Language.LSP.Types.Utils

data DefinitionClientCapabilities =
  DefinitionClientCapabilities
    { -- | Whether definition supports dynamic registration.
      _dynamicRegistration :: Maybe Bool
      -- | The client supports additional metadata in the form of definition
      -- links.
      -- Since LSP 3.14.0
    , _linkSupport :: Maybe Bool
    } deriving (Show, Read, Eq)
deriveJSON lspOptions ''DefinitionClientCapabilities

makeExtendingDatatype "DefinitionOptions" [''WorkDoneProgressOptions] []
deriveJSON lspOptions ''DefinitionOptions

makeExtendingDatatype "DefinitionRegistrationOptions"
  [ ''TextDocumentRegistrationOptions
  , ''DefinitionOptions
  ] []
deriveJSON lspOptions ''DefinitionRegistrationOptions

makeExtendingDatatype "DefinitionParams"
  [ ''TextDocumentPositionParams
  , ''WorkDoneProgressParams
  , ''PartialResultParams
  ] []
deriveJSON lspOptions ''DefinitionParams
