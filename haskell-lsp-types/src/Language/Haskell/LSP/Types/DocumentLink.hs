{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Haskell.LSP.Types.DocumentLink where

import Data.Aeson
import Data.Aeson.TH
import Language.Haskell.LSP.Types.Location
import Language.Haskell.LSP.Types.Progress
import Language.Haskell.LSP.Types.TextDocument
import Language.Haskell.LSP.Types.Uri
import Language.Haskell.LSP.Types.Utils

data DocumentLinkClientCapabilities =
  DocumentLinkClientCapabilities
  { -- | Whether document link supports dynamic registration.
    _dynamicRegistration :: Maybe Bool
    -- | Whether the client supports the `tooltip` property on `DocumentLink`.
    --
    -- Since LSP 3.15.0
  , _tooltipSupport :: Maybe Bool
  } deriving (Read, Show, Eq)
deriveJSON lspOptions ''DocumentLinkClientCapabilities

-- -------------------------------------

makeExtendingDatatype "DocumentLinkOptions" [''WorkDoneProgressOptions]
  [("_resolveProvider", [t| Maybe Bool |])]
deriveJSON lspOptions ''DocumentLinkOptions

makeExtendingDatatype "DocumentLinkRegistrationOptions"
  [ ''TextDocumentRegistrationOptions
  , ''DocumentLinkOptions
  ] []
deriveJSON lspOptions ''DocumentLinkRegistrationOptions

-- -------------------------------------

makeExtendingDatatype "DocumentLinkParams"
  [ ''WorkDoneProgressParams
  , ''PartialResultParams
  ]
  [("_textDocument", [t| TextDocumentIdentifier |])]
deriveJSON lspOptions ''DocumentLinkParams

-- -------------------------------------

-- | A document link is a range in a text document that links to an internal or
-- external resource, like another text document or a web site.
data DocumentLink =
  DocumentLink
  { -- | The range this link applies to.
    _range :: Range
    -- | The uri this link points to. If missing a resolve request is sent
    -- later.
  , _target :: Maybe Uri
    -- | The tooltip text when you hover over this link.
    --
    -- If a tooltip is provided, is will be displayed in a string that includes
    -- instructions on how to trigger the link, such as @{0} (ctrl + click)@.
    -- The specific instructions vary depending on OS, user settings, and
    -- localization.
    --
    -- Since LSP 3.15.0
  , _tooltip :: Maybe String
    -- | A data entry field that is preserved on a document link between a
    -- DocumentLinkRequest and a DocumentLinkResolveRequest.
  , _xdata :: Maybe Value
  } deriving (Read, Show, Eq)
deriveJSON lspOptions ''DocumentLink
