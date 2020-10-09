{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

module Language.Haskell.LSP.Types.Declaration where

import Data.Aeson.TH
import Language.Haskell.LSP.Types.Progress
import Language.Haskell.LSP.Types.StaticRegistrationOptions
import Language.Haskell.LSP.Types.TextDocument
import Language.Haskell.LSP.Types.Utils

data DeclarationClientCapabilities =
  DeclarationClientCapabilities
  { -- | Whether declaration supports dynamic registration. If this is set to 'true'
    -- the client supports the new 'DeclarationRegistrationOptions' return value
    -- for the corresponding server capability as well.
    _dynamicRegistration :: Maybe Bool
    -- | The client supports additional metadata in the form of declaration links.
  , _linkSupport :: Maybe Bool
  }
  deriving (Read, Show, Eq)
deriveJSON lspOptions ''DeclarationClientCapabilities

makeExtendingDatatype "DeclarationOptions" [''WorkDoneProgressOptions] []
deriveJSON lspOptions ''DeclarationOptions

makeExtendingDatatype "DeclarationRegistrationOptions"
  [ ''DeclarationOptions
  , ''TextDocumentRegistrationOptions
  , ''StaticRegistrationOptions
  ] []
deriveJSON lspOptions ''DeclarationRegistrationOptions

makeExtendingDatatype "DeclarationParams"
  [ ''TextDocumentPositionParams
  , ''WorkDoneProgressParams
  , ''PartialResultParams
  ] []
deriveJSON lspOptions ''DeclarationParams
