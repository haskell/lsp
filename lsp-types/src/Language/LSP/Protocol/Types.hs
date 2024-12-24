-- The OSPath import sometimes looks unused
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.LSP.Protocol.Types (
  -- * Basic types and functions
  module Common,

  -- ** URIs
  module Uri,

  -- ** Locations
  module Locations,

  -- ** LSP enumerations
  module LspEnum,

  -- ** Singleton types
  module Singletons,

  -- * Helpers for working with LSP types

  -- ** Edits
  module Edits,

  -- ** Markup
  module Markup,

  -- ** Code actions
  module CodeAction,

  -- ** Semantic tokens
  module SemanticTokens,

  -- ** WatchKinds
  module WatchKinds,

  -- * Main LSP types and functions
  module Generated,
) where

import Language.LSP.Protocol.Internal.Types as Generated
import Language.LSP.Protocol.Types.CodeAction as CodeAction
import Language.LSP.Protocol.Types.Common as Common
import Language.LSP.Protocol.Types.Edit as Edits
import Language.LSP.Protocol.Types.Location as Locations
import Language.LSP.Protocol.Types.LspEnum as LspEnum
import Language.LSP.Protocol.Types.MarkupContent as Markup
import Language.LSP.Protocol.Types.Orphans ()
import Language.LSP.Protocol.Types.SemanticTokens as SemanticTokens
import Language.LSP.Protocol.Types.Singletons as Singletons
import Language.LSP.Protocol.Types.Uri as Uri
import Language.LSP.Protocol.Types.Uri.OsPath as Uri
import Language.LSP.Protocol.Types.WatchKinds as WatchKinds
