-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.CodeLensOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Language.LSP.Protocol.Types.Common

{-|
Code Lens provider options of a `CodeLensRequest`.
-}
data CodeLensOptions = CodeLensOptions 
  { {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  , {-|
  Code lens has a resolve provider as well.
  -}
  _resolveProvider :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON CodeLensOptions)

instance Aeson.ToJSON CodeLensOptions where
  toJSON (CodeLensOptions arg0 arg1) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,"resolveProvider" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON CodeLensOptions where
  parseJSON = Aeson.withObject "CodeLensOptions" $ \arg -> CodeLensOptions <$> arg Aeson..:! "workDoneProgress" <*> arg Aeson..:! "resolveProvider"
