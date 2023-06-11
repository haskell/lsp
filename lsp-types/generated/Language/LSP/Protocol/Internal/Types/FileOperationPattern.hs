-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.FileOperationPattern where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.FileOperationPatternKind
import qualified Language.LSP.Protocol.Internal.Types.FileOperationPatternOptions
import qualified Language.LSP.Protocol.Types.Common

{-|
A pattern to describe in which file operation requests or notifications
the server is interested in receiving.

@since 3.16.0
-}
data FileOperationPattern = FileOperationPattern 
  { {-|
  The glob pattern to match. Glob patterns can have the following syntax:
  - `*` to match one or more characters in a path segment
  - `?` to match on one character in a path segment
  - `**` to match any number of path segments, including none
  - `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
  - `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
  - `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)
  -}
  _glob :: Data.Text.Text
  , {-|
  Whether to match files or folders with this pattern.

  Matches both if undefined.
  -}
  _matches :: (Maybe Language.LSP.Protocol.Internal.Types.FileOperationPatternKind.FileOperationPatternKind)
  , {-|
  Additional options used during matching.
  -}
  _options :: (Maybe Language.LSP.Protocol.Internal.Types.FileOperationPatternOptions.FileOperationPatternOptions)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

instance Aeson.ToJSON FileOperationPattern where
  toJSON (FileOperationPattern arg0 arg1 arg2) = Aeson.object $ concat $  [["glob" Aeson..= arg0]
    ,"matches" Language.LSP.Protocol.Types.Common..=? arg1
    ,"options" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON FileOperationPattern where
  parseJSON = Aeson.withObject "FileOperationPattern" $ \arg -> FileOperationPattern <$> arg Aeson..: "glob" <*> arg Aeson..:! "matches" <*> arg Aeson..:! "options"
