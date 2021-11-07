{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Language.LSP.Types.DocumentColor where

import Data.Aeson.TH
import Data.Text (Text)
import Language.LSP.Types.Common
import Language.LSP.Types.Location
import Language.LSP.Types.Progress
import Language.LSP.Types.StaticRegistrationOptions
import Language.LSP.Types.TextDocument
import Language.LSP.Types.Utils
import Language.LSP.Types.WorkspaceEdit

data DocumentColorClientCapabilities =
  DocumentColorClientCapabilities
  { -- | Whether document color supports dynamic registration.
    _dynamicRegistration :: Maybe Bool
  } deriving (Read, Show, Eq)
deriveJSON lspOptions ''DocumentColorClientCapabilities

-- -------------------------------------

makeExtendingDatatype "DocumentColorOptions" [''WorkDoneProgressOptions] []
deriveJSON lspOptions ''DocumentColorOptions

makeExtendingDatatype "DocumentColorRegistrationOptions"
  [ ''TextDocumentRegistrationOptions
  , ''StaticRegistrationOptions
  , ''DocumentColorOptions
  ] []
deriveJSON lspOptions ''DocumentColorRegistrationOptions

-- -------------------------------------

makeExtendingDatatype "DocumentColorParams"
  [ ''WorkDoneProgressParams
  , ''PartialResultParams
  ]
  [("_textDocument", [t| TextDocumentIdentifier |])]
deriveJSON lspOptions ''DocumentColorParams

-- -------------------------------------

-- | Represents a color in RGBA space.
data Color =
  Color
    { _red   :: Float -- ^ The red component of this color in the range [0-1].
    , _green :: Float -- ^ The green component of this color in the range [0-1].
    , _blue  :: Float -- ^ The blue component of this color in the range [0-1].
    , _alpha :: Float -- ^ The alpha component of this color in the range [0-1].
    } deriving (Read, Show, Eq)
deriveJSON lspOptions ''Color

data ColorInformation =
  ColorInformation
    { _range :: Range -- ^ The range in the document where this color appears.
    , _color :: Color -- ^ The actual color value for this color range.
    } deriving (Read, Show, Eq)
deriveJSON lspOptions ''ColorInformation

-- -------------------------------------

makeExtendingDatatype "ColorPresentationParams"
  [ ''WorkDoneProgressParams
  , ''PartialResultParams
  ]
  [ ("_textDocument", [t| TextDocumentIdentifier |])
  , ("_color", [t| Color |])
  , ("_range", [t| Range |])
  ]
deriveJSON lspOptions ''ColorPresentationParams

-- -------------------------------------

data ColorPresentation =
  ColorPresentation
    { -- | The label of this color presentation. It will be shown on the color
      -- picker header. By default this is also the text that is inserted when selecting
      -- this color presentation.
      _label               :: Text
      -- | A 'TextEdit' which is applied to a document when selecting
      -- this presentation for the color.  When `falsy` the '_label'
      -- is used.
    , _textEdit            :: Maybe TextEdit
      -- | An optional array of additional 'TextEdit's that are applied when
      -- selecting this color presentation. Edits must not overlap with the main
      -- '_textEdit' nor with themselves.
    , _additionalTextEdits :: Maybe (List TextEdit)
    } deriving (Read, Show, Eq)
deriveJSON lspOptions ''ColorPresentation
