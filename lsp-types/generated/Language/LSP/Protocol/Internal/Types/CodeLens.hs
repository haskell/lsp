-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CodeLens where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.Command
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
A code lens represents a `Command` that should be shown along with
source text, like the number of references, a way to run tests, etc.

A code lens is _unresolved_ when no command is associated to it. For performance
reasons the creation of a code lens and resolving should be done in two stages.

-}
data CodeLens = CodeLens 
  { {-|
  The range in which this code lens is valid. Should only span a single line.

  -}
  _range :: Language.LSP.Protocol.Internal.Types.Range.Range
  , {-|
  The command this code lens represents.

  -}
  _command :: (Maybe Language.LSP.Protocol.Internal.Types.Command.Command)
  , {-|
  A data entry field that is preserved on a code lens item between
  a `CodeLensRequest` and a `CodeLensResolveRequest`

  -}
  _data_ :: (Maybe Data.Aeson.Value)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON CodeLens where
  toJSON (CodeLens arg0 arg1 arg2) = Aeson.object $ concat $  [["range" Aeson..= arg0]
    ,"command" Language.LSP.Protocol.Types.Common..=? arg1
    ,"data" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON CodeLens where
  parseJSON = Aeson.withObject "CodeLens" $ \arg -> CodeLens <$> arg Aeson..: "range" <*> arg Aeson..:! "command" <*> arg Aeson..:! "data"