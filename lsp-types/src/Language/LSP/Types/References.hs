{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | Find References Request
-- https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_references
module Language.LSP.Types.References where

import Data.Aeson.TH

import Language.LSP.Types.TextDocument
import Language.LSP.Types.Progress
import Language.LSP.Types.Utils

data ReferencesClientCapabilities =
  ReferencesClientCapabilities
    { -- | Whether references supports dynamic registration.
      _dynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)
deriveJSON lspOptions ''ReferencesClientCapabilities

makeExtendingDatatype "ReferenceOptions" [''WorkDoneProgressOptions] []
deriveJSON lspOptions ''ReferenceOptions

makeExtendingDatatype "ReferenceRegistrationOptions"
  [ ''TextDocumentRegistrationOptions
  , ''ReferenceOptions
  ]
  []
deriveJSON lspOptions ''ReferenceRegistrationOptions

data ReferenceContext =
  ReferenceContext
    { -- | Include the declaration of the current symbol.
      _includeDeclaration :: Bool
    } deriving (Read,Show,Eq)
deriveJSON lspOptions ''ReferenceContext

makeExtendingDatatype "ReferenceParams"
  [ ''TextDocumentPositionParams
  , ''WorkDoneProgressParams
  , ''PartialResultParams
  ]
  [("_context", [t| ReferenceContext |])]
deriveJSON lspOptions ''ReferenceParams
