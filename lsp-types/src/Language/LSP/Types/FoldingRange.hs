{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.LSP.Types.FoldingRange where

import qualified Data.Aeson                    as A
import           Data.Aeson.TH
import           Data.Text                    (Text)
import           Language.LSP.Types.Progress
import           Language.LSP.Types.StaticRegistrationOptions
import           Language.LSP.Types.TextDocument
import           Language.LSP.Types.Utils


-- -------------------------------------

data FoldingRangeClientCapabilities =
  FoldingRangeClientCapabilities
    { -- | Whether implementation supports dynamic registration for folding range
      -- providers. If this is set to `true` the client supports the new
      -- `(FoldingRangeProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions)`
      -- return value for the corresponding server capability as well.
      _dynamicRegistration :: Maybe Bool
      -- | The maximum number of folding ranges that the client prefers to receive
      -- per document. The value serves as a hint, servers are free to follow the limit.
    , _rangeLimit          :: Maybe Int
      -- | If set, the client signals that it only supports folding complete lines. If set,
      -- client will ignore specified `startCharacter` and `endCharacter` properties in a
      -- FoldingRange.
    , _lineFoldingOnly     :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''FoldingRangeClientCapabilities

makeExtendingDatatype "FoldingRangeOptions" [''WorkDoneProgressOptions] []
deriveJSON lspOptions ''FoldingRangeOptions

makeExtendingDatatype "FoldingRangeRegistrationOptions"
  [ ''TextDocumentRegistrationOptions
  , ''FoldingRangeOptions
  , ''StaticRegistrationOptions
  ] []
deriveJSON lspOptions ''FoldingRangeRegistrationOptions


makeExtendingDatatype "FoldingRangeParams"
  [ ''WorkDoneProgressParams
  , ''PartialResultParams
  ]
  [("_textDocument", [t| TextDocumentIdentifier |])]
deriveJSON lspOptions ''FoldingRangeParams

-- | Enum of known range kinds
data FoldingRangeKind = FoldingRangeComment
                        -- ^ Folding range for a comment
                      | FoldingRangeImports
                        -- ^ Folding range for a imports or includes
                      | FoldingRangeRegion
                        -- ^ Folding range for a region (e.g. #region)
                      | FoldingRangeUnknown Text
                        -- ^ Folding range that haskell-lsp-types does
                        -- not yet support
  deriving (Read, Show, Eq)

instance A.ToJSON FoldingRangeKind where
  toJSON FoldingRangeComment     = A.String "comment"
  toJSON FoldingRangeImports     = A.String "imports"
  toJSON FoldingRangeRegion      = A.String "region"
  toJSON (FoldingRangeUnknown x) = A.String x

instance A.FromJSON FoldingRangeKind where
  parseJSON (A.String "comment") = pure FoldingRangeComment
  parseJSON (A.String "imports") = pure FoldingRangeImports
  parseJSON (A.String "region")  = pure FoldingRangeRegion
  parseJSON (A.String x)         = pure (FoldingRangeUnknown x)
  parseJSON _                    = mempty

-- | Represents a folding range.
data FoldingRange =
  FoldingRange
  { -- | The zero-based line number from where the folded range starts.
    _startLine      :: Int
    -- | The zero-based character offset from where the folded range
    -- starts. If not defined, defaults to the length of the start line.
  , _startCharacter :: Maybe Int
    -- | The zero-based line number where the folded range ends.
  , _endLine        :: Int
    -- | The zero-based character offset before the folded range ends.
    -- If not defined, defaults to the length of the end line.
  , _endCharacter   :: Maybe Int
    -- | Describes the kind of the folding range such as 'comment' or
    -- 'region'. The kind is used to categorize folding ranges and used
    -- by commands like 'Fold all comments'. See 'FoldingRangeKind' for
    -- an enumeration of standardized kinds.
  , _kind           :: Maybe FoldingRangeKind
  }
  deriving (Read, Show, Eq)

deriveJSON lspOptions ''FoldingRange
