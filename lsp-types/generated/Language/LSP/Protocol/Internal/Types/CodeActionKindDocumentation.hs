{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CodeActionKindDocumentation where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.CodeActionKind
import qualified Language.LSP.Protocol.Internal.Types.Command
import qualified Language.LSP.Protocol.Types.Common

{-|
Documentation for a class of code actions.

@since 3.18.0
-}
data CodeActionKindDocumentation = CodeActionKindDocumentation 
  { {-|
  The kind of the code action being documented.

  If the kind is generic, such as `CodeActionKind.Refactor`, the documentation will be shown whenever any
  refactorings are returned. If the kind if more specific, such as `CodeActionKind.RefactorExtract`, the
  documentation will only be shown when extract refactoring code actions are returned.
  -}
  _kind :: Language.LSP.Protocol.Internal.Types.CodeActionKind.CodeActionKind
  , {-|
  Command that is ued to display the documentation to the user.

  The title of this documentation code action is taken from {@linkcode Command.title}
  -}
  _command :: Language.LSP.Protocol.Internal.Types.Command.Command
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CodeActionKindDocumentation)

instance Aeson.ToJSON CodeActionKindDocumentation where
  toJSON (CodeActionKindDocumentation arg0 arg1) = Aeson.object $ concat $  [["kind" Aeson..= arg0]
    ,["command" Aeson..= arg1]]

instance Aeson.FromJSON CodeActionKindDocumentation where
  parseJSON = Aeson.withObject "CodeActionKindDocumentation" $ \arg -> CodeActionKindDocumentation <$> arg Aeson..: "kind" <*> arg Aeson..: "command"
