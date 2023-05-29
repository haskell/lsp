-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.Range where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Internal.Types.Position
import qualified Language.LSP.Protocol.Types.Common

{-|
A range in a text document expressed as (zero-based) start and end positions.

If you want to specify a range that contains a line including the line ending
character(s) then use an end position denoting the start of the next line.
For example:
```ts
{
    start: { line: 5, character: 23 }
    end : { line 6, character : 0 }
}
```

-}
data Range = Range 
  { {-|
  The range's start position.

  -}
  _start :: Language.LSP.Protocol.Internal.Types.Position.Position
  , {-|
  The range's end position.

  -}
  _end :: Language.LSP.Protocol.Internal.Types.Position.Position
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON Range where
  toJSON (Range arg0 arg1) = Aeson.object $ concat $  [["start" Aeson..= arg0]
    ,["end" Aeson..= arg1]]

instance Aeson.FromJSON Range where
  parseJSON = Aeson.withObject "Range" $ \arg -> Range <$> arg Aeson..: "start" <*> arg Aeson..: "end"