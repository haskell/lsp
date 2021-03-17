{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.LSP.Types.WorkspaceSymbol where

import Data.Aeson.TH
import Data.Default
import Language.LSP.Types.Common
import Language.LSP.Types.DocumentSymbol
import Language.LSP.Types.Progress
import Language.LSP.Types.Utils
import Data.Text (Text)

data WorkspaceSymbolKindClientCapabilities =
  WorkspaceSymbolKindClientCapabilities
   { -- | The symbol kind values the client supports. When this
     -- property exists the client also guarantees that it will
     -- handle values outside its set gracefully and falls back
     -- to a default value when unknown.
     --
     -- If this property is not present the client only supports
     -- the symbol kinds from `File` to `Array` as defined in
     -- the initial version of the protocol.
     _valueSet :: Maybe (List SymbolKind)
   } deriving (Show, Read, Eq)

deriveJSON lspOptions ''WorkspaceSymbolKindClientCapabilities

data WorkspaceSymbolTagClientCapabilities =
  WorkspaceSymbolTagClientCapabilities
    { -- | The tags supported by the client.
      _valueSet :: Maybe (List SymbolTag)
    }
  deriving (Show, Read, Eq)

deriveJSON lspOptions ''WorkspaceSymbolTagClientCapabilities

instance Default WorkspaceSymbolKindClientCapabilities where
  def = WorkspaceSymbolKindClientCapabilities (Just $ List allKinds)
    where allKinds = [ SkFile
                     , SkModule
                     , SkNamespace
                     , SkPackage
                     , SkClass
                     , SkMethod
                     , SkProperty
                     , SkField
                     , SkConstructor
                     , SkEnum
                     , SkInterface
                     , SkFunction
                     , SkVariable
                     , SkConstant
                     , SkString
                     , SkNumber
                     , SkBoolean
                     , SkArray
                     ]

data WorkspaceSymbolClientCapabilities =
  WorkspaceSymbolClientCapabilities
    { _dynamicRegistration :: Maybe Bool -- ^Symbol request supports dynamic
                                         -- registration.
    , _symbolKind :: Maybe WorkspaceSymbolKindClientCapabilities -- ^ Specific capabilities for the `SymbolKind`.
      -- | The client supports tags on `SymbolInformation`.
      -- Clients supporting tags have to handle unknown tags gracefully.
      --
      -- @since 3.16.0
    , _tagSupport :: Maybe WorkspaceSymbolTagClientCapabilities 
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''WorkspaceSymbolClientCapabilities

-- -------------------------------------

makeExtendingDatatype "WorkspaceSymbolOptions" [''WorkDoneProgressOptions] []
deriveJSON lspOptions ''WorkspaceSymbolOptions

makeExtendingDatatype "WorkspaceSymbolRegistrationOptions"
  [''WorkspaceSymbolOptions] []
deriveJSON lspOptions ''WorkspaceSymbolRegistrationOptions

-- -------------------------------------

makeExtendingDatatype "WorkspaceSymbolParams"
  [ ''WorkDoneProgressParams
  , ''PartialResultParams
  ]
  [("_query", [t| Text |])]

deriveJSON lspOptions ''WorkspaceSymbolParams
