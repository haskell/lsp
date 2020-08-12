{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | Find References Request
-- https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_references
module Language.Haskell.LSP.Types.References where

import Data.Aeson.TH

import Language.Haskell.LSP.Types.TextDocument
import Language.Haskell.LSP.Types.Progress
import Language.Haskell.LSP.Types.Utils

data ReferencesClientCapabilities =
  ReferencesClientCapabilities
    { _dynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''ReferencesClientCapabilities)

data ReferenceContext =
  ReferenceContext
    { -- | Include the declaration of the current symbol.
      _includeDeclaration :: Bool
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''ReferenceContext

makeExtendingDatatype "ReferenceOptions" [''WorkDoneProgressOptions] []
deriveJSON lspOptions ''ReferenceOptions

makeExtendingDatatype "ReferenceRegistrationOptions"
  [ ''TextDocumentRegistrationOptions
  , ''ReferenceOptions
  ]
  []
deriveJSON lspOptions ''ReferenceRegistrationOptions

makeExtendingDatatype "ReferenceParams"
  [ ''TextDocumentPositionParams
  , ''WorkDoneProgressParams
  , ''PartialResultParams
  ]
  [("_context", [t| ReferenceContext |])]
deriveJSON lspOptions ''ReferenceParams
