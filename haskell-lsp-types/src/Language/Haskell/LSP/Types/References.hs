{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Language.Haskell.LSP.Types.References where

import Data.Aeson.TH
import Language.Haskell.LSP.Types.Constants
import Language.Haskell.LSP.Types.TextDocument
import Language.Haskell.LSP.Types.Progress
import Language.Haskell.LSP.Types.Utils

{-
Find References Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#find-references-request

The references request is sent from the client to the server to resolve
project-wide references for the symbol denoted by the given text document
position.

    Changed: In 2.0 the request uses TextDocumentPositionParams with proper
    textDocument and position properties. In 1.0 the uri of the referenced text
    document was inlined into the params object.

Request

    method: 'textDocument/references'
    params: ReferenceParams defined as follows:

interface ReferenceParams extends TextDocumentPositionParams {
    context: ReferenceContext
}

interface ReferenceContext {
    /**
     * Include the declaration of the current symbol.
     */
    includeDeclaration: boolean;
}

Response:

    result: Location[]
    error: code and message set in case an exception happens during the
           reference request.
-}

data ReferenceContext =
  ReferenceContext
    { -- | Include the declaration of the current symbol.
      _includeDeclaration :: Bool
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''ReferenceContext

data ReferenceParams =
  ReferenceParams
    { _textDocumentPositionParams :: TextDocumentPositionParams
    , _workDoneProgressParams     :: WorkDoneProgressParams
    , _partialResultParams        :: PartialResultParams
    , _context                    :: ReferenceContext
    } deriving (Read,Show,Eq)

deriveJSONExtendFields lspOptions ''ReferenceParams
  [ "_textDocumentPositionParams"
  , "_workDoneProgressParams"
  , "_partialResultParams"
  ]

data ReferenceOptions =
  ReferenceOptions
    { _workDoneProgressOptions :: WorkDoneProgressOptions
    } deriving (Read,Show,Eq)
deriveJSONExtendFields lspOptions ''ReferenceOptions ["_workDoneProgressOptions"]

data ReferenceRegistrationOptions =
  ReferenceRegistrationOptions
    { _textDocumentRegistrationOptions :: TextDocumentRegistrationOptions
    , _referenceOptions                :: ReferenceOptions
    } deriving (Read,Show,Eq)
deriveJSONExtendFields lspOptions ''ReferenceRegistrationOptions
  [ "_textDocumentRegistrationOptions"
  , "_referenceOptions"
  ]
