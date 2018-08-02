{-# LANGUAGE TemplateHaskell            #-}
module Language.Haskell.LSP.TH.Symbol where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text                                      (Text)
import           Language.Haskell.LSP.TH.Constants
import           Language.Haskell.LSP.TH.TextDocument
import           Language.Haskell.LSP.TH.List
import           Language.Haskell.LSP.TH.Location
import           Language.Haskell.LSP.TH.Message

-- ---------------------------------------------------------------------
{-
Document Symbols Request

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#document-symbols-request

The document symbol request is sent from the client to the server to list all
symbols found in a given text document.

    Changed: In 2.0 the request uses DocumentSymbolParams instead of a single
             uri.

Request

    method: 'textDocument/documentSymbol'
    params: DocumentSymbolParams defined as follows:

interface DocumentSymbolParams {
    /**
     * The text document.
     */
    textDocument: TextDocumentIdentifier;
}

Response

    result: SymbolInformation[] defined as follows:

/**
 * Represents information about programming constructs like variables, classes,
 * interfaces etc.
 */
interface SymbolInformation {
    /**
     * The name of this symbol.
     */
    name: string;

    /**
     * The kind of this symbol.
     */
    kind: number;

    /**
     * The location of this symbol.
     */
    location: Location;

    /**
     * The name of the symbol containing this symbol.
     */
    containerName?: string;
}

Where the kind is defined like this:

/**
 * A symbol kind.
 */
export enum SymbolKind {
    File = 1,
    Module = 2,
    Namespace = 3,
    Package = 4,
    Class = 5,
    Method = 6,
    Property = 7,
    Field = 8,
    Constructor = 9,
    Enum = 10,
    Interface = 11,
    Function = 12,
    Variable = 13,
    Constant = 14,
    Text = 15,
    Number = 16,
    Boolean = 17,
    Array = 18,
}

    error: code and message set in case an exception happens during the document
           symbol request.

Registration Options: TextDocumentRegistrationOptions
-}

data DocumentSymbolParams =
  DocumentSymbolParams
    { _textDocument :: TextDocumentIdentifier
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DocumentSymbolParams

-- -------------------------------------

data SymbolKind
    = SkFile
    | SkModule
    | SkNamespace
    | SkPackage
    | SkClass
    | SkMethod
    | SkProperty
    | SkField
    | SkConstructor
    | SkEnum
    | SkInterface
    | SkFunction
    | SkVariable
    | SkConstant
    | SkString
    | SkNumber
    | SkBoolean
    | SkArray
    | SkObject
    | SkKey
    | SkNull
    | SkEnumMember
    | SkStruct
    | SkEvent
    | SkOperator
    | SkTypeParameter
    deriving (Read,Show,Eq)

instance ToJSON SymbolKind where
  toJSON SkFile          = Number 1
  toJSON SkModule        = Number 2
  toJSON SkNamespace     = Number 3
  toJSON SkPackage       = Number 4
  toJSON SkClass         = Number 5
  toJSON SkMethod        = Number 6
  toJSON SkProperty      = Number 7
  toJSON SkField         = Number 8
  toJSON SkConstructor   = Number 9
  toJSON SkEnum          = Number 10
  toJSON SkInterface     = Number 11
  toJSON SkFunction      = Number 12
  toJSON SkVariable      = Number 13
  toJSON SkConstant      = Number 14
  toJSON SkString        = Number 15
  toJSON SkNumber        = Number 16
  toJSON SkBoolean       = Number 17
  toJSON SkArray         = Number 18
  toJSON SkObject        = Number 19
  toJSON SkKey           = Number 20
  toJSON SkNull          = Number 21
  toJSON SkEnumMember    = Number 22
  toJSON SkStruct        = Number 23
  toJSON SkEvent         = Number 24
  toJSON SkOperator      = Number 25
  toJSON SkTypeParameter = Number 26


instance FromJSON SymbolKind where
  parseJSON (Number  1) = pure SkFile
  parseJSON (Number  2) = pure SkModule
  parseJSON (Number  3) = pure SkNamespace
  parseJSON (Number  4) = pure SkPackage
  parseJSON (Number  5) = pure SkClass
  parseJSON (Number  6) = pure SkMethod
  parseJSON (Number  7) = pure SkProperty
  parseJSON (Number  8) = pure SkField
  parseJSON (Number  9) = pure SkConstructor
  parseJSON (Number 10) = pure SkEnum
  parseJSON (Number 11) = pure SkInterface
  parseJSON (Number 12) = pure SkFunction
  parseJSON (Number 13) = pure SkVariable
  parseJSON (Number 14) = pure SkConstant
  parseJSON (Number 15) = pure SkString
  parseJSON (Number 16) = pure SkNumber
  parseJSON (Number 17) = pure SkBoolean
  parseJSON (Number 18) = pure SkArray
  parseJSON (Number 19) = pure SkObject
  parseJSON (Number 20) = pure SkKey
  parseJSON (Number 21) = pure SkNull
  parseJSON (Number 22) = pure SkEnumMember
  parseJSON (Number 23) = pure SkStruct
  parseJSON (Number 24) = pure SkEvent
  parseJSON (Number 25) = pure SkOperator
  parseJSON (Number 26) = pure SkTypeParameter
  parseJSON _             = mempty


-- ---------------------------------------------------------------------

data SymbolInformation =
  SymbolInformation
    { _name          :: Text
    , _kind          :: SymbolKind
    , _location      :: Location
    , _containerName :: Maybe Text -- ^The name of the symbol containing this
                                     -- symbol.
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''SymbolInformation

-- -------------------------------------

type DocumentSymbolRequest = RequestMessage ClientMethod DocumentSymbolParams (List SymbolInformation)
type DocumentSymbolsResponse = ResponseMessage (List SymbolInformation)
