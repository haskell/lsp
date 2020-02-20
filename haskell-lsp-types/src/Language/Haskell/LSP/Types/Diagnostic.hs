{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Language.Haskell.LSP.Types.Diagnostic where

import           Control.DeepSeq
import qualified Data.Aeson                                 as A
import           Data.Aeson.TH
import           Data.Scientific
import           Data.Text
import           GHC.Generics
import           Language.Haskell.LSP.Types.Constants
import           Language.Haskell.LSP.Types.List
import           Language.Haskell.LSP.Types.Location

-- ---------------------------------------------------------------------
{-
The protocol currently supports the following diagnostic severities:

enum DiagnosticSeverity {
    /**
     * Reports an error.
     */
    Error = 1,
    /**
     * Reports a warning.
     */
    Warning = 2,
    /**
     * Reports an information.
     */
    Information = 3,
    /**
     * Reports a hint.
     */
    Hint = 4
}
-}
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

{-
The diagnostic tags.

export namespace DiagnosticTag {
    /**
     * Unused or unnecessary code.
     *
     * Clients are allowed to render diagnostics with this tag faded out instead of having
     * an error squiggle.
     */
    export const Unnecessary: 1;
    /**
     * Deprecated or obsolete code.
     *
     * Clients are allowed to rendered diagnostics with this tag strike through.
     */
    export const Deprecated: 2;
}
-}
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
{-
Represents a related message and source code location for a diagnostic. This should be
used to point to code locations that cause or related to a diagnostics, e.g when duplicating
a symbol in a scope.

export interface DiagnosticRelatedInformation {
  /**
   * The location of this related diagnostic information.
   */
  location: Location;

  /**
   * The message of this related diagnostic information.
   */
  message: string;
}
-}

data DiagnosticRelatedInformation =
  DiagnosticRelatedInformation
    { _location :: Location
    , _message  :: Text
    } deriving (Show, Read, Eq, Ord, Generic)

instance NFData DiagnosticRelatedInformation

deriveJSON lspOptions ''DiagnosticRelatedInformation

-- ---------------------------------------------------------------------
{-
Diagnostic

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#diagnostic

Represents a diagnostic, such as a compiler error or warning. Diagnostic objects
are only valid in the scope of a resource.

interface Diagnostic {
    /**
     * The range at which the message applies.
     */
    range: Range;

    /**
     * The diagnostic's severity. Can be omitted. If omitted it is up to the
     * client to interpret diagnostics as error, warning, info or hint.
     */
    severity?: number;

    /**
     * The diagnostic's code. Can be omitted.
     */
    code?: number | string;

    /**
     * A human-readable string describing the source of this
     * diagnostic, e.g. 'typescript' or 'super lint'.
     */
    source?: string;

    /**
     * The diagnostic's message.
     */
    message: string;

    /**
     * Additional metadata about the diagnostic.
     *
     * @since 3.15.0
     */
    tags?: DiagnosticTag[];

    /**
     * An array of related diagnostic information, e.g. when symbol-names within
     * a scope collide all definitions can be marked via this property.
     */
    relatedInformation?: DiagnosticRelatedInformation[];
}
-}

data NumberOrString =
  NumberValue Scientific
  | StringValue Text
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData NumberOrString

deriveJSON lspOptions { sumEncoding = A.UntaggedValue } ''NumberOrString

type DiagnosticSource = Text
data Diagnostic =
  Diagnostic
    { _range              :: Range
    , _severity           :: Maybe DiagnosticSeverity
    , _code               :: Maybe NumberOrString
    , _source             :: Maybe DiagnosticSource
    , _message            :: Text
    , _tags               :: Maybe (List DiagnosticTag)
    , _relatedInformation :: Maybe (List DiagnosticRelatedInformation)
    } deriving (Show, Read, Eq, Ord, Generic)

instance NFData Diagnostic

deriveJSON lspOptions ''Diagnostic
