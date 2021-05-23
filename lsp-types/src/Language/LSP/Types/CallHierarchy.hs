{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

{- | Since LSP 3.16.0 -}
module Language.LSP.Types.CallHierarchy where

import Data.Aeson.TH
import Data.Aeson.Types ( Value )
import Data.Text ( Text )

import Language.LSP.Types.Progress
import Language.LSP.Types.TextDocument
import Language.LSP.Types.StaticRegistrationOptions
import Language.LSP.Types.DocumentSymbol
import Language.LSP.Types.Uri
import Language.LSP.Types.Location
import Language.LSP.Types.Common
import Language.LSP.Types.Utils


data CallHierarchyClientCapabilities =
  CallHierarchyClientCapabilities
    { _dynamicRegistration :: Maybe Bool }
    deriving (Show, Read, Eq)
deriveJSON lspOptions ''CallHierarchyClientCapabilities

makeExtendingDatatype "CallHierarchyOptions" [''WorkDoneProgressOptions] []
deriveJSON lspOptions ''CallHierarchyOptions

makeExtendingDatatype "CallHierarchyRegistrartionOptions"
  [ ''TextDocumentRegistrationOptions
  , ''CallHierarchyOptions
  , ''StaticRegistrationOptions
  ]
  []
deriveJSON lspOptions ''CallHierarchyRegistrartionOptions

makeExtendingDatatype "CallHierarchyPrepareParams"
  [''TextDocumentPositionParams, ''WorkDoneProgressParams] []
deriveJSON lspOptions ''CallHierarchyPrepareParams

data CallHierarchyItem =
  CallHierarchyItem
    { _name :: Text
    , _kind :: SymbolKind
    , _tags :: Maybe (List SymbolTag)
    -- | More detail for this item, e.g. the signature of a function.
    , _detail :: Maybe Text
    , _uri :: Uri
    , _range :: Range
    -- | The range that should be selected and revealed when this symbol
    -- is being picked, e.g. the name of a function. Must be contained by
    -- the @_range@.
    , _selectionRange :: Range
    -- | A data entry field that is preserved between a call hierarchy
    -- prepare and incoming calls or outgoing calls requests.
    , _xdata :: Maybe Value
    }
    deriving (Show, Read, Eq)
deriveJSON lspOptions ''CallHierarchyItem

-- -------------------------------------

makeExtendingDatatype "CallHierarchyIncomingCallsParams"
  [ ''WorkDoneProgressParams
  , ''PartialResultParams
  ]
  [("_item", [t| CallHierarchyItem |])]
deriveJSON lspOptions ''CallHierarchyIncomingCallsParams

data CallHierarchyIncomingCall =
  CallHierarchyIncomingCall
    { -- | The item that makes the call.
      _from :: CallHierarchyItem
    -- | The ranges at which the calls appear. This is relative to the caller
    -- denoted by @_from@.
    , _fromRanges :: List Range
    }
    deriving (Show, Read, Eq)
deriveJSON lspOptions ''CallHierarchyIncomingCall

-- -------------------------------------

makeExtendingDatatype "CallHierarchyOutgoingCallsParams"
  [ ''WorkDoneProgressParams
  , ''PartialResultParams
  ]
  [("_item", [t| CallHierarchyItem |])]
deriveJSON lspOptions ''CallHierarchyOutgoingCallsParams

data CallHierarchyOutgoingCall =
  CallHierarchyOutgoingCall
    { -- | The item that is called.
      _to :: CallHierarchyItem
    -- | The range at which this item is called. THis is the range relative to
    -- the caller, e.g the item passed to `callHierarchy/outgoingCalls` request.
    , _fromRanges :: List Range
    }
    deriving (Show, Read, Eq)
deriveJSON lspOptions ''CallHierarchyOutgoingCall
