{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.LSP.Types.SelectionRange where

import Data.Aeson.TH
import Language.Haskell.LSP.Types.Common
import Language.Haskell.LSP.Types.Location
import Language.Haskell.LSP.Types.Progress
import Language.Haskell.LSP.Types.StaticRegistrationOptions
import Language.Haskell.LSP.Types.TextDocument
import Language.Haskell.LSP.Types.Utils

data SelectionRangeClientCapabilities = SelectionRangeClientCapabilities
  { -- | Whether implementation supports dynamic registration for selection range providers. If this is set to 'True'
    -- the client supports the new 'SelectionRangeRegistrationOptions' return value for the corresponding server
    -- capability as well.
    _dynamicRegistration :: Maybe Bool
  }
  deriving (Read, Show, Eq)

deriveJSON lspOptions ''SelectionRangeClientCapabilities

makeExtendingDatatype "SelectionRangeOptions" [''WorkDoneProgressOptions] []
deriveJSON lspOptions ''SelectionRangeOptions

makeExtendingDatatype
  "SelectionRangeRegistrationOptions"
  [ ''SelectionRangeOptions,
    ''TextDocumentRegistrationOptions,
    ''StaticRegistrationOptions
  ]
  []
deriveJSON lspOptions ''SelectionRangeRegistrationOptions

makeExtendingDatatype
  "SelectionRangeParams"
  [ ''WorkDoneProgressParams,
    ''PartialResultParams
  ]
  [ ("_textDocument", [t|TextDocumentIdentifier|]),
    ("_positions", [t|List Position|])
  ]
deriveJSON lspOptions ''SelectionRangeParams

data SelectionRange = SelectionRange
  { -- | The 'range' of this selection range.
    _range :: Range,
    -- | The parent selection range containing this range. Therefore @parent.range@ must contain @this.range@.
    _parent :: Maybe SelectionRange
  }
  deriving (Read, Show, Eq)

deriveJSON lspOptions ''SelectionRange
