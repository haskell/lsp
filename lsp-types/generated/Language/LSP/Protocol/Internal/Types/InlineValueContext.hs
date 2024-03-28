{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.InlineValueContext where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Internal.Types.Range
import qualified Language.LSP.Protocol.Types.Common

{-|
@since 3.17.0
-}
data InlineValueContext = InlineValueContext 
  { {-|
  The stack frame (as a DAP Id) where the execution has stopped.
  -}
  frameId :: Language.LSP.Protocol.Types.Common.Int32
  , {-|
  The document range where execution has stopped.
  Typically the end position of the range denotes the line where the inline values are shown.
  -}
  stoppedLocation :: Language.LSP.Protocol.Internal.Types.Range.Range
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON InlineValueContext)

instance Aeson.ToJSON InlineValueContext where
  toJSON (InlineValueContext arg0 arg1) = Aeson.object $ concat $  [["frameId" Aeson..= arg0]
    ,["stoppedLocation" Aeson..= arg1]]

instance Aeson.FromJSON InlineValueContext where
  parseJSON = Aeson.withObject "InlineValueContext" $ \arg -> InlineValueContext <$> arg Aeson..: "frameId" <*> arg Aeson..: "stoppedLocation"
