{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CodeActionOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.CodeActionKind
import qualified Language.LSP.Protocol.Types.Common

{-|
Provider options for a `CodeActionRequest`.
-}
data CodeActionOptions = CodeActionOptions 
  { {-|

  -}
  workDoneProgress :: (Maybe Bool)
  , {-|
  CodeActionKinds that this server may return.

  The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
  may list out every specific kind they provide.
  -}
  codeActionKinds :: (Maybe [Language.LSP.Protocol.Internal.Types.CodeActionKind.CodeActionKind])
  , {-|
  The server provides support to resolve additional
  information for a code action.

  @since 3.16.0
  -}
  resolveProvider :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CodeActionOptions)

instance Aeson.ToJSON CodeActionOptions where
  toJSON (CodeActionOptions arg0 arg1 arg2) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,"codeActionKinds" Language.LSP.Protocol.Types.Common..=? arg1
    ,"resolveProvider" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON CodeActionOptions where
  parseJSON = Aeson.withObject "CodeActionOptions" $ \arg -> CodeActionOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "workDoneProgress" <*> arg Language.LSP.Protocol.Types.Common..:!? "codeActionKinds" <*> arg Language.LSP.Protocol.Types.Common..:!? "resolveProvider"
