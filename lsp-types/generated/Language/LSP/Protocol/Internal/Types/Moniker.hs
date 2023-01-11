-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.Moniker where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.MonikerKind
import qualified Language.LSP.Protocol.Internal.Types.UniquenessLevel
import qualified Language.LSP.Protocol.Types.Common

{-|
Moniker definition to match LSIF 0.5 moniker definition.

@since 3.16.0

-}
data Moniker = Moniker 
  { {-|
  The scheme of the moniker. For example tsc or .Net

  -}
  _scheme :: Data.Text.Text
  , {-|
  The identifier of the moniker. The value is opaque in LSIF however
  schema owners are allowed to define the structure if they want.

  -}
  _identifier :: Data.Text.Text
  , {-|
  The scope in which the moniker is unique

  -}
  _unique :: Language.LSP.Protocol.Internal.Types.UniquenessLevel.UniquenessLevel
  , {-|
  The moniker kind if known.

  -}
  _kind :: (Maybe Language.LSP.Protocol.Internal.Types.MonikerKind.MonikerKind)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Aeson.ToJSON Moniker where
  toJSON (Moniker arg0 arg1 arg2 arg3) = Aeson.object $ concat $  [["scheme" Aeson..= arg0]
    ,["identifier" Aeson..= arg1]
    ,["unique" Aeson..= arg2]
    ,"kind" Language.LSP.Protocol.Types.Common..=? arg3]

instance Aeson.FromJSON Moniker where
  parseJSON = Aeson.withObject "Moniker" $ \arg -> Moniker <$> arg Aeson..: "scheme" <*> arg Aeson..: "identifier" <*> arg Aeson..: "unique" <*> arg Aeson..:! "kind"