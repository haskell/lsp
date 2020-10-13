{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.LSP.Types.Rename where

import Data.Aeson.TH
import Data.Text (Text)

import Language.LSP.Types.Location
import Language.LSP.Types.TextDocument
import Language.LSP.Types.Progress
import Language.LSP.Types.Utils

data RenameClientCapabilities =
  RenameClientCapabilities
    { -- | Whether rename supports dynamic registration.
      _dynamicRegistration :: Maybe Bool
      -- | Client supports testing for validity of rename operations
      -- before execution.
      --
      -- Since LSP 3.12.0
    , _prepareSupport :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''RenameClientCapabilities

makeExtendingDatatype "RenameOptions" [''WorkDoneProgressOptions]
  [("_prepareProvider", [t| Maybe Bool |])]
deriveJSON lspOptions ''RenameOptions

makeExtendingDatatype "RenameRegistrationOptions"
  [ ''TextDocumentRegistrationOptions
  , ''RenameOptions
  ] []
deriveJSON lspOptions ''RenameRegistrationOptions

makeExtendingDatatype "RenameParams"
  [ ''TextDocumentPositionParams
  , ''WorkDoneProgressParams
  ]
  [("_newName", [t| Text |])]
deriveJSON lspOptions ''RenameParams

-- -----------------------------------------

makeExtendingDatatype "PrepareRenameParams" [''TextDocumentPositionParams] []
deriveJSON lspOptions ''PrepareRenameParams

data RangeWithPlaceholder =
  RangeWithPlaceholder
  {
    _range :: Range
  , _placeholder :: Text
  } deriving Eq
deriveJSON lspOptions ''RangeWithPlaceholder
