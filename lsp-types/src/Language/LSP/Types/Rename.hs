{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.LSP.Types.Rename where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.Scientific (Scientific)

import Language.LSP.Types.Location
import Language.LSP.Types.TextDocument
import Language.LSP.Types.Progress
import Language.LSP.Types.Utils

data PrepareSupportDefaultBehavior =
  PsIdentifier |
  PsUnknown Scientific
  deriving (Read, Show, Eq)

instance ToJSON PrepareSupportDefaultBehavior where
  toJSON PsIdentifier  = Number 1
  toJSON (PsUnknown i) = Number i

instance FromJSON PrepareSupportDefaultBehavior where
  parseJSON (Number 1) = pure PsIdentifier
  parseJSON _          = mempty

data RenameClientCapabilities =
  RenameClientCapabilities
    { -- | Whether rename supports dynamic registration.
      _dynamicRegistration :: Maybe Bool
      -- | Client supports testing for validity of rename operations
      -- before execution.
      --
      -- Since LSP 3.12.0
    , _prepareSupport :: Maybe Bool
      -- | Client supports the default behavior result
      -- (`{ defaultBehavior: boolean }`).
      --
      -- The value indicates the default behavior used by the client.
      --
      -- @since 3.16.0
    , prepareSupportDefaultBehavior :: Maybe PrepareSupportDefaultBehavior
      -- | Whether the client honors the change annotations in
      -- text edits and resource operations returned via the
      -- rename request's workspace edit by for example presenting
      -- the workspace edit in the user interface and asking
      -- for confirmation.
      --
      -- @since 3.16.0
    , honorsChangeAnnotations :: Maybe Bool
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
