{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Language.Haskell.LSP.Types.Symbol where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Scientific
import           Data.Text                                      (Text)
import           Language.Haskell.LSP.Types.Constants
import           Language.Haskell.LSP.Types.TextDocument
import           Language.Haskell.LSP.Types.List
import           Language.Haskell.LSP.Types.Location
import           Language.Haskell.LSP.Types.Message
import           Language.Haskell.LSP.Types.Progress

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
    , _workDoneToken :: Maybe ProgressToken -- ^ An optional token that a server can use to report work done progress.
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
    | SkUnknown Scientific
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
  toJSON (SkUnknown x)   = Number x

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
  parseJSON (Number x)  = pure (SkUnknown x)
  parseJSON _           = mempty

-- ---------------------------------------------------------------------

-- | Represents programming constructs like variables, classes, interfaces etc.
-- that appear in a document. Document symbols can be hierarchical and they
-- have two ranges: one that encloses its definition and one that points to its
-- most interesting range, e.g. the range of an identifier.
data DocumentSymbol =
  DocumentSymbol
    { _name           :: Text -- ^ The name of this symbol.
    -- | More detail for this symbol, e.g the signature of a function. If not
    -- provided the name is used.
    , _detail         :: Maybe Text
    , _kind           :: SymbolKind -- ^ The kind of this symbol.
    , _deprecated     :: Maybe Bool -- ^ Indicates if this symbol is deprecated.
    -- | The range enclosing this symbol not including leading/trailing
    -- whitespace but everything else like comments. This information is
    -- typically used to determine if the the clients cursor is inside the symbol
    -- to reveal in the symbol in the UI.
    , _range          :: Range
    -- | The range that should be selected and revealed when this symbol is being
    -- picked, e.g the name of a function. Must be contained by the the '_range'.
    , _selectionRange :: Range
    -- | Children of this symbol, e.g. properties of a class.
    , _children       :: Maybe (List DocumentSymbol)
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''DocumentSymbol

-- ---------------------------------------------------------------------

-- | Represents information about programming constructs like variables, classes,
-- interfaces etc.
data SymbolInformation =
  SymbolInformation
    { _name          :: Text -- ^ The name of this symbol.
    , _kind          :: SymbolKind -- ^ The kind of this symbol.
    , _deprecated    :: Maybe Bool -- ^ Indicates if this symbol is deprecated.
    -- | The location of this symbol. The location's range is used by a tool
    -- to reveal the location in the editor. If the symbol is selected in the
    -- tool the range's start information is used to position the cursor. So
    -- the range usually spans more then the actual symbol's name and does
    -- normally include things like visibility modifiers.
    --
    -- The range doesn't have to denote a node range in the sense of a abstract
    -- syntax tree. It can therefore not be used to re-construct a hierarchy of
    -- the symbols.
    , _location      :: Location
    -- | The name of the symbol containing this symbol. This information is for
    -- user interface purposes (e.g. to render a qualifier in the user interface
    -- if necessary). It can't be used to re-infer a hierarchy for the document
    -- symbols.
    , _containerName :: Maybe Text
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''SymbolInformation

-- -------------------------------------

data DSResult = DSDocumentSymbols (List DocumentSymbol)
              | DSSymbolInformation (List SymbolInformation)
  deriving (Read,Show,Eq)

instance FromJSON DSResult where
  parseJSON x = DSDocumentSymbols <$> parseJSON x <|> DSSymbolInformation <$> parseJSON x

instance ToJSON DSResult where
  toJSON (DSDocumentSymbols x) = toJSON x
  toJSON (DSSymbolInformation x) = toJSON x


