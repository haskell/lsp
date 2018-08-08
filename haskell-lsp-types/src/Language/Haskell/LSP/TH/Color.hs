{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.Haskell.LSP.TH.Color where

import           Data.Aeson.TH
import           Data.Text (Text)
import           Language.Haskell.LSP.TH.Constants
import           Language.Haskell.LSP.TH.List
import           Language.Haskell.LSP.TH.Location
import           Language.Haskell.LSP.TH.Message
import           Language.Haskell.LSP.TH.TextDocument
import           Language.Haskell.LSP.TH.WorkspaceEdit

{-
Document Color Request (:leftwards_arrow_with_hook:)
Since version 3.6.0

The document color request is sent from the client to the server to list all color references found in a given text document. Along with the range, a color value in RGB is returned.

Clients can use the result to decorate color references in an editor. For example:

Color boxes showing the actual color next to the reference
Show a color picker when a color reference is edited
Request:

method: ‘textDocument/documentColor’
params: DocumentColorParams defined as follows
interface DocumentColorParams {
	/**
	 * The text document.
	 */
	textDocument: TextDocumentIdentifier;
}
Response:

result: ColorInformation[] defined as follows:
interface ColorInformation {
	/**
	 * The range in the document where this color appears.
	 */
	range: Range;

	/**
	 * The actual color value for this color range.
	 */
	color: Color;
}

/**
 * Represents a color in RGBA space.
 */
interface Color {

	/**
	 * The red component of this color in the range [0-1].
	 */
	readonly red: number;

	/**
	 * The green component of this color in the range [0-1].
	 */
	readonly green: number;

	/**
	 * The blue component of this color in the range [0-1].
	 */
	readonly blue: number;

	/**
	 * The alpha component of this color in the range [0-1].
	 */
	readonly alpha: number;
}
error: code and message set in case an exception happens during the ‘textDocument/documentColor’ request
-}

-- | Represents a color in RGBA space.
data Color =
  Color
    { _red   :: Int -- ^ The red component of this color in the range [0-1].
    , _green :: Int -- ^ The green component of this color in the range [0-1].
    , _blue  :: Int -- ^ The blue component of this color in the range [0-1].
    , _alpha :: Int -- ^ The alpha component of this color in the range [0-1].
    } deriving (Read, Show, Eq)

deriveJSON lspOptions ''Color

data ColorInformation =
  ColorInformation
    { _range :: Range -- ^ The range in the document where this color appears.
    , _color :: Color -- ^ The actual color value for this color range.
    } deriving (Read, Show, Eq)

deriveJSON lspOptions ''ColorInformation

data DocumentColorParams =
  DocumentColorParams
    { _textDocument :: TextDocumentIdentifier -- ^ The text document.
    } deriving (Read, Show, Eq)

deriveJSON lspOptions ''DocumentColorParams

type DocumentColorRequest = RequestMessage ClientMethod DocumentColorParams (List ColorInformation)
type DocumentColorResponse = ResponseMessage (List ColorInformation)

{-
Color Presentation Request (:leftwards_arrow_with_hook:)
Since version 3.6.0

The color presentation request is sent from the client to the server to obtain a list of presentations for a color value at a given location. Clients can use the result to

modify a color reference.
show in a color picker and let users pick one of the presentations
Request:

method: ‘textDocument/colorPresentation’
params: DocumentColorParams defined as follows
interface ColorPresentationParams {
	/**
	 * The text document.
	 */
	textDocument: TextDocumentIdentifier;

	/**
	 * The color information to request presentations for.
	 */
	color: Color;

	/**
	 * The range where the color would be inserted. Serves as a context.
	 */
	range: Range;
}
Response:

result: ColorPresentation[] defined as follows:
interface ColorPresentation {
	/**
	 * The label of this color presentation. It will be shown on the color
	 * picker header. By default this is also the text that is inserted when selecting
	 * this color presentation.
	 */
	label: string;
	/**
	 * An [edit](#TextEdit) which is applied to a document when selecting
	 * this presentation for the color.  When `falsy` the [label](#ColorPresentation.label)
	 * is used.
	 */
	textEdit?: TextEdit;
	/**
	 * An optional array of additional [text edits](#TextEdit) that are applied when
	 * selecting this color presentation. Edits must not overlap with the main [edit](#ColorPresentation.textEdit) nor with themselves.
	 */
	additionalTextEdits?: TextEdit[];
}
error: code and message set in case an exception happens during the ‘textDocument/colorPresentation’ request
-}

data ColorPresentationParams =
  ColorPresentationParams
    { -- | The text document.
      _textDocument :: TextDocumentIdentifier
      -- | The color information to request presentations for.
    , _color        :: Color
      -- | The range where the color would be inserted.
      -- Serves as a context.
    , _range        :: Range
    } deriving (Read, Show, Eq)

deriveJSON lspOptions ''ColorPresentationParams

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

type ColorPresentationRequest = RequestMessage ClientMethod ColorPresentationParams (List ColorPresentation)
type ColorPresentationResponse = ResponseMessage (List ColorPresentation)
