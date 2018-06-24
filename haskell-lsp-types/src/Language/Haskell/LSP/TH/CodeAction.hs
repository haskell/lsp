{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.Haskell.LSP.TH.CodeAction where

import           Control.Applicative
import qualified Data.Aeson                                 as A
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Language.Haskell.LSP.TH.Command
import           Language.Haskell.LSP.TH.Constants
import           Language.Haskell.LSP.TH.Diagnostic
import           Language.Haskell.LSP.TH.List
import           Language.Haskell.LSP.TH.Location
import           Language.Haskell.LSP.TH.Message
import           Language.Haskell.LSP.TH.TextDocumentIdentifier
  

{-
Code Action Request
https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#code-action-request
The code action request is sent from the client to the server to compute
commands for a given text document and range. The request is triggered when the
user moves the cursor into a problem marker in the editor or presses the
lightbulb associated with a marker.
Request
    method: 'textDocument/codeAction'
    params: CodeActionParams defined as follows:
/**
 * Params for the CodeActionRequest
 */
interface CodeActionParams {
    /**
     * The document in which the command was invoked.
     */
    textDocument: TextDocumentIdentifier;
    /**
     * The range for which the command was invoked.
     */
    range: Range;
    /**
     * Context carrying additional information.
     */
    context: CodeActionContext;
}
/**
 * Contains additional diagnostic information about the context in which
 * a code action is run.
 */
interface CodeActionContext {
    /**
     * An array of diagnostics.
     */
    diagnostics: Diagnostic[];
}
Response
    result: Command[] defined as follows:
    error: code and message set in case an exception happens during the code
           action request.
-}

data CodeActionContext =
  CodeActionContext
    { _diagnostics :: List Diagnostic
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''CodeActionContext

data CodeActionParams =
  CodeActionParams
    { _textDocument :: TextDocumentIdentifier
    , _range        :: Range
    , _context      :: CodeActionContext
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''CodeActionParams


type CodeActionRequest  = RequestMessage ClientMethod CodeActionParams (List Command)
type CodeActionResponse = ResponseMessage (List Command)