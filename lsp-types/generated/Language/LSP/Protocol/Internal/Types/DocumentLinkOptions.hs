-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DocumentLinkOptions where

import Control.DeepSeq
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Language.LSP.Protocol.Types.Common

{-|
Provider options for a `DocumentLinkRequest`.

-}
data DocumentLinkOptions = DocumentLinkOptions 
  { {-|

  -}
  _workDoneProgress :: (Maybe Bool)
  , {-|
  Document links have a resolve provider as well.

  -}
  _resolveProvider :: (Maybe Bool)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON DocumentLinkOptions where
  toJSON (DocumentLinkOptions arg0 arg1) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,"resolveProvider" Language.LSP.Protocol.Types.Common..=? arg1]

instance Aeson.FromJSON DocumentLinkOptions where
  parseJSON = Aeson.withObject "DocumentLinkOptions" $ \arg -> DocumentLinkOptions <$> arg Aeson..:! "workDoneProgress" <*> arg Aeson..:! "resolveProvider"