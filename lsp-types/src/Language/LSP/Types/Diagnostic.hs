{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeOperators              #-}

module Language.LSP.Types.Diagnostic where

import           Control.DeepSeq
import qualified Data.Aeson                                 as A
import           Data.Aeson.TH
import           Data.Text
import           GHC.Generics
import           Language.LSP.Types.Common
import           Language.LSP.Types.Location
import           Language.LSP.Types.Uri
import           Language.LSP.Types.Utils

-- ---------------------------------------------------------------------

data DiagnosticSeverity
  = DsError   -- ^ Error = 1,
  | DsWarning -- ^ Warning = 2,
  | DsInfo    -- ^ Info = 3,
  | DsHint    -- ^ Hint = 4
  deriving (Eq,Ord,Show,Read, Generic)

instance NFData DiagnosticSeverity

instance A.ToJSON DiagnosticSeverity where
  toJSON DsError   = A.Number 1
  toJSON DsWarning = A.Number 2
  toJSON DsInfo    = A.Number 3
  toJSON DsHint    = A.Number 4

instance A.FromJSON DiagnosticSeverity where
  parseJSON (A.Number 1) = pure DsError
  parseJSON (A.Number 2) = pure DsWarning
  parseJSON (A.Number 3) = pure DsInfo
  parseJSON (A.Number 4) = pure DsHint
  parseJSON _            = mempty

data DiagnosticTag
  -- | Unused or unnecessary code.
  --
  -- Clients are allowed to render diagnostics with this tag faded out
  -- instead of having an error squiggle.
  = DtUnnecessary
  -- | Deprecated or obsolete code.
  --
  -- Clients are allowed to rendered diagnostics with this tag strike
  -- through.
  | DtDeprecated
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData DiagnosticTag

instance A.ToJSON DiagnosticTag where
  toJSON DtUnnecessary = A.Number 1
  toJSON DtDeprecated  = A.Number 2

instance A.FromJSON DiagnosticTag where
  parseJSON (A.Number 1) = pure DtUnnecessary
  parseJSON (A.Number 2) = pure DtDeprecated
  parseJSON _            = mempty

-- ---------------------------------------------------------------------

data DiagnosticRelatedInformation =
  DiagnosticRelatedInformation
    { _location :: Location
    , _message  :: Text
    } deriving (Show, Read, Eq, Ord, Generic)

instance NFData DiagnosticRelatedInformation

deriveJSON lspOptions ''DiagnosticRelatedInformation

-- ---------------------------------------------------------------------

type DiagnosticSource = Text
data Diagnostic =
  Diagnostic
    { _range              :: Range
    , _severity           :: Maybe DiagnosticSeverity
    , _code               :: Maybe (Int |? Text)
    , _source             :: Maybe DiagnosticSource
    , _message            :: Text
    , _tags               :: Maybe (List DiagnosticTag)
    , _relatedInformation :: Maybe (List DiagnosticRelatedInformation)
    } deriving (Show, Read, Eq, Ord, Generic)

instance NFData Diagnostic

deriveJSON lspOptions ''Diagnostic

-- -------------------------------------

data PublishDiagnosticsTagsClientCapabilities =
  PublishDiagnosticsTagsClientCapabilities
    { -- | The tags supported by the client.
      _valueSet :: List DiagnosticTag
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''PublishDiagnosticsTagsClientCapabilities

data PublishDiagnosticsClientCapabilities =
  PublishDiagnosticsClientCapabilities
    { -- | Whether the clients accepts diagnostics with related information.
      _relatedInformation :: Maybe Bool
      -- | Client supports the tag property to provide metadata about a
      -- diagnostic.
      --
      -- Clients supporting tags have to handle unknown tags gracefully.
      -- 
      -- Since LSP 3.15.0
    , _tagSupport :: Maybe PublishDiagnosticsTagsClientCapabilities
      -- | Whether the client interprets the version property of the
      -- @textDocument/publishDiagnostics@ notification's parameter.
      -- 
      -- Since LSP 3.15.0
    , _versionSupport :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''PublishDiagnosticsClientCapabilities

data PublishDiagnosticsParams =
  PublishDiagnosticsParams
    { -- | The URI for which diagnostic information is reported.
      _uri         :: Uri
      -- | Optional the version number of the document the diagnostics are
      -- published for.
      -- 
      -- Since LSP 3.15.0
    , _version     :: Maybe Int
      -- | An array of diagnostic information items.
    , _diagnostics :: List Diagnostic
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''PublishDiagnosticsParams
