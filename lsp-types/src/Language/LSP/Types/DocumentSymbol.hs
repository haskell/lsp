{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Language.LSP.Types.DocumentSymbol where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Scientific
import           Data.Text                                      (Text)

import           Language.LSP.Types.TextDocument
import           Language.LSP.Types.Common
import           Language.LSP.Types.Location
import           Language.LSP.Types.Progress
import           Language.LSP.Types.Utils

-- ---------------------------------------------------------------------

makeExtendingDatatype "DocumentSymbolOptions"
  [''WorkDoneProgressOptions]
  [ ("_label", [t| Maybe Bool |])]
deriveJSON lspOptions ''DocumentSymbolOptions

makeExtendingDatatype "DocumentSymbolRegistrationOptions"
  [ ''TextDocumentRegistrationOptions
  , ''DocumentSymbolOptions
  ] []
deriveJSON lspOptions ''DocumentSymbolRegistrationOptions

-- ---------------------------------------------------------------------

makeExtendingDatatype "DocumentSymbolParams"
  [ ''WorkDoneProgressParams
  , ''PartialResultParams
  ]
  [ ("_textDocument", [t| TextDocumentIdentifier |])]
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
  parseJSON _           = fail "SymbolKind"

{-|
Symbol tags are extra annotations that tweak the rendering of a symbol.

@since 3.16.0
-}
data SymbolTag =
  StDeprecated -- ^ Render a symbol as obsolete, usually using a strike-out.
  | StUnknown Scientific
  deriving (Read, Show, Eq)

instance ToJSON SymbolTag where
  toJSON StDeprecated          = Number 1
  toJSON (StUnknown x)   = Number x

instance FromJSON SymbolTag where
  parseJSON (Number  1) = pure StDeprecated
  parseJSON (Number x)  = pure (StUnknown x)
  parseJSON _           = fail "SymbolTag"
  
-- -------------------------------------

data DocumentSymbolKindClientCapabilities =
  DocumentSymbolKindClientCapabilities
    { -- | The symbol kind values the client supports. When this
      --  property exists the client also guarantees that it will
      --  handle values outside its set gracefully and falls back
      --  to a default value when unknown.
      --
      --  If this property is not present the client only supports
      --  the symbol kinds from `File` to `Array` as defined in
      --  the initial version of the protocol.
      _valueSet :: Maybe (List SymbolKind)
    }
  deriving (Show, Read, Eq)

deriveJSON lspOptions ''DocumentSymbolKindClientCapabilities

data DocumentSymbolTagClientCapabilities =
  DocumentSymbolTagClientCapabilities
    { -- | The tags supported by the client.
      _valueSet :: Maybe (List SymbolTag)
    }
  deriving (Show, Read, Eq)

deriveJSON lspOptions ''DocumentSymbolTagClientCapabilities

data DocumentSymbolClientCapabilities =
  DocumentSymbolClientCapabilities
    { -- | Whether document symbol supports dynamic registration.
      _dynamicRegistration :: Maybe Bool
      -- | Specific capabilities for the `SymbolKind`.
    , _symbolKind :: Maybe DocumentSymbolKindClientCapabilities
    , _hierarchicalDocumentSymbolSupport :: Maybe Bool
      -- | The client supports tags on `SymbolInformation`.
      -- Clients supporting tags have to handle unknown tags gracefully.
      --
      -- @since 3.16.0
    , _tagSupport :: Maybe DocumentSymbolTagClientCapabilities
      -- | The client supports an additional label presented in the UI when
      -- registering a document symbol provider.
      --
      -- @since 3.16.0
    , _labelSupport :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DocumentSymbolClientCapabilities

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
    , _tags           :: Maybe (List SymbolTag) -- ^ Tags for this document symbol.
    , _deprecated     :: Maybe Bool -- ^ Indicates if this symbol is deprecated. Deprecated, use tags instead.
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
    , _tags          :: Maybe (List SymbolTag) -- ^ Tags for this symbol.
    , _deprecated    :: Maybe Bool -- ^ Indicates if this symbol is deprecated. Deprecated, use tags instead.
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
{-# DEPRECATED _deprecated "Use tags instead" #-}

deriveJSON lspOptions ''SymbolInformation
