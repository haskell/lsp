{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Haskell.LSP.Types.CodeLens where

import Data.Aeson
import Data.Aeson.TH
import Language.Haskell.LSP.Types.Command
import Language.Haskell.LSP.Types.Location
import Language.Haskell.LSP.Types.Progress
import Language.Haskell.LSP.Types.TextDocument
import Language.Haskell.LSP.Types.Utils

-- -------------------------------------

data CodeLensClientCapabilities =
  CodeLensClientCapabilities
    { -- | Whether code lens supports dynamic registration.
      _dynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''CodeLensClientCapabilities

-- -------------------------------------

makeExtendingDatatype "CodeLensOptions" [''WorkDoneProgressOptions]
  [ ("_resolveProvider", [t| Maybe Bool |] )]
deriveJSON lspOptions ''CodeLensOptions

makeExtendingDatatype "CodeLensRegistrationOptions" 
  [ ''TextDocumentRegistrationOptions
  , ''CodeLensOptions
  ] []
deriveJSON lspOptions ''CodeLensRegistrationOptions

-- -------------------------------------

makeExtendingDatatype "CodeLensParams"
  [ ''WorkDoneProgressParams,
    ''PartialResultParams
  ]
  [("_textDocument", [t|TextDocumentIdentifier|])]
deriveJSON lspOptions ''CodeLensParams

-- -------------------------------------

-- | A code lens represents a command that should be shown along with source
-- text, like the number of references, a way to run tests, etc.
-- 
-- A code lens is _unresolved_ when no command is associated to it. For
-- performance reasons the creation of a code lens and resolving should be done
-- in two stages.
data CodeLens =
  CodeLens
  { -- | The range in which this code lens is valid. Should only span a single line.
    _range   :: Range
  , -- | The command this code lens represents.
    _command :: Maybe Command
  , -- | A data entry field that is preserved on a code lens item between
    -- a code lens and a code lens resolve request.
    _xdata   :: Maybe Value
  } deriving (Read,Show,Eq)

deriveJSON lspOptions ''CodeLens
