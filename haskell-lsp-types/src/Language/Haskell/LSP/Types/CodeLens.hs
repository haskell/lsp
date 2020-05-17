{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.LSP.Types.CodeLens where

import Data.Aeson
import Data.Aeson.TH
import Language.Haskell.LSP.Types.Command
import Language.Haskell.LSP.Types.Constants
import Language.Haskell.LSP.Types.Location
import Language.Haskell.LSP.Types.Progress
import Language.Haskell.LSP.Types.TextDocument
import Language.Haskell.LSP.Types.Utils

{-

The code lens request is sent from the client to the server to compute code lenses for a given text document.

Client Capability:

property name (optional): textDocument.codeLens
property type: CodeLensClientCapabilities defined as follows:
export interface CodeLensClientCapabilities {
	/**
	 * Whether code lens supports dynamic registration.
	 */
	dynamicRegistration?: boolean;
}
Server Capability:

property name (optional): codeLensProvider
property type: CodeLensOptions defined as follows:
export interface CodeLensOptions extends WorkDoneProgressOptions {
	/**
	 * Code lens has a resolve provider as well.
	 */
	resolveProvider?: boolean;
}
Registration Options: CodeLensRegistrationOptions defined as follows:

export interface CodeLensRegistrationOptions extends TextDocumentRegistrationOptions, CodeLensOptions {
}
Request:

method: ‘textDocument/codeLens’
params: CodeLensParams defined as follows:
interface CodeLensParams extends WorkDoneProgressParams, PartialResultParams {
	/**
	 * The document to request code lens for.
	 */
	textDocument: TextDocumentIdentifier;
}
Response:

result: CodeLens[] | null defined as follows:
/**
 * A code lens represents a command that should be shown along with
 * source text, like the number of references, a way to run tests, etc.
 *
 * A code lens is _unresolved_ when no command is associated to it. For performance
 * reasons the creation of a code lens and resolving should be done in two stages.
 */
interface CodeLens {
	/**
	 * The range in which this code lens is valid. Should only span a single line.
	 */
	range: Range;

	/**
	 * The command this code lens represents.
	 */
	command?: Command;

	/**
	 * A data entry field that is preserved on a code lens item between
	 * a code lens and a code lens resolve request.
	 */
	data?: any
}
partial result: CodeLens[]
error: code and message set in case an exception happens during the code lens request.

-}

data CodeLensParams =
  CodeLensParams
    { _textDocument :: TextDocumentIdentifier
    , _workDoneToken :: Maybe ProgressToken -- ^ An optional token that a server can use to report work done progress.
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''CodeLensParams


-- ---------------------------------------------------------------------


data CodeLens =
  CodeLens
    { _range   :: Range
    , _command :: Maybe Command
    , _xdata   :: Maybe Value
    } deriving (Read,Show,Eq)

deriveJSON lspOptions{ fieldLabelModifier = customModifier } ''CodeLens


-- ---------------------------------------------------------------------
{-
/**
 * Code Lens options.
 */
export interface CodeLensOptions extends WorkDoneProgressOptions {
	/**
	 * Code lens has a resolve provider as well.
	 */
	resolveProvider?: boolean;
}
-}

data CodeLensOptions =
  CodeLensOptions
    { _workDoneProgressOptions :: WorkDoneProgressOptions
    -- | Code lens has a resolve provider as well.
    , _resolveProvider         :: Maybe Bool
    } deriving (Read,Show,Eq)
deriveJSONExtendFields lspOptions ''CodeLensOptions [ "_workDoneProgressOptions" ]

{-
Registration Options: CodeLensRegistrationOptions defined as follows:

export interface CodeLensRegistrationOptions extends TextDocumentRegistrationOptions, CodeLensOptions {
}
-}

data CodeLensRegistrationOptions =
  CodeLensRegistrationOptions
    { _documentSelector :: TextDocumentRegistrationOptions
    , _codeLensOptions  :: CodeLensOptions
    } deriving (Show, Read, Eq)

deriveJSONExtendFields lspOptions ''CodeLensRegistrationOptions
  [ "_documentSelector"
  , "_codeLensOptions"
  ]

-- ---------------------------------------------------------------------
{-
Code Lens Resolve Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#code-lens-resolve-request

The code lens resolve request is sent from the client to the server to resolve
the command for a given code lens item.

Request

    method: 'codeLens/resolve'
    params: CodeLens

Response

    result: CodeLens
    error: code and message set in case an exception happens during the code
           lens resolve request.


-}
