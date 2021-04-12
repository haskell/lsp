{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.LSP.Types.DocumentHighlight where

import Data.Aeson
import Data.Aeson.TH
import Language.LSP.Types.Location
import Language.LSP.Types.Progress
import Language.LSP.Types.TextDocument
import Language.LSP.Types.Utils

-- -------------------------------------

data DocumentHighlightClientCapabilities =
  DocumentHighlightClientCapabilities
    { -- | Whether document highlight supports dynamic registration.
      _dynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DocumentHighlightClientCapabilities

makeExtendingDatatype "DocumentHighlightOptions" [''WorkDoneProgressOptions] []
deriveJSON lspOptions ''DocumentHighlightOptions

makeExtendingDatatype "DocumentHighlightRegistrationOptions"
  [ ''TextDocumentRegistrationOptions
  , ''DocumentHighlightOptions
  ] []
deriveJSON lspOptions ''DocumentHighlightRegistrationOptions

makeExtendingDatatype "DocumentHighlightParams"
  [ ''TextDocumentPositionParams
  , ''WorkDoneProgressParams
  , ''PartialResultParams
  ] []
deriveJSON lspOptions ''DocumentHighlightParams

data DocumentHighlightKind
  = -- | A textual occurrence.
    HkText
  | -- | Read-access of a symbol, like reading a variable.
    HkRead
  | -- | Write-access of a symbol, like writing to a variable.
    HkWrite
  deriving (Read, Show, Eq)

instance ToJSON DocumentHighlightKind where
  toJSON HkText  = Number 1
  toJSON HkRead  = Number 2
  toJSON HkWrite = Number 3

instance FromJSON DocumentHighlightKind where
  parseJSON (Number 1) = pure HkText
  parseJSON (Number 2) = pure HkRead
  parseJSON (Number 3) = pure HkWrite
  parseJSON _          = mempty "DocumentHighlightKind"

-- -------------------------------------

-- | A document highlight is a range inside a text document which deserves
-- special attention. Usually a document highlight is visualized by changing the
-- background color of its range.
data DocumentHighlight =
  DocumentHighlight
    { -- | The range this highlight applies to.
      _range :: Range
      -- | The highlight kind, default is 'HkText'.
    , _kind  :: Maybe DocumentHighlightKind
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DocumentHighlight
