{- ORMOLU_DISABLE -}
{- HLINT ignore -}
-- THIS IS A GENERATED FILE, DO NOT EDIT

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Language.LSP.Protocol.Internal.Types.DeclarationRegistrationOptions where

import Control.DeepSeq
import Data.Hashable
import GHC.Generics
import Language.LSP.Protocol.Utils.Misc
import Prettyprinter
import qualified Data.Aeson as Aeson
import qualified Data.Row.Aeson as Aeson
import qualified Data.Row.Hashable as Hashable
import qualified Data.Text
import qualified Language.LSP.Protocol.Internal.Types.DocumentSelector
import qualified Language.LSP.Protocol.Types.Common

{-|

-}
data DeclarationRegistrationOptions = DeclarationRegistrationOptions 
  { {-|

  -}
  workDoneProgress :: (Maybe Bool)
  , {-|
  A document selector to identify the scope of the registration. If set to null
  the document selector provided on the client side will be used.
  -}
  documentSelector :: (Language.LSP.Protocol.Internal.Types.DocumentSelector.DocumentSelector Language.LSP.Protocol.Types.Common.|? Language.LSP.Protocol.Types.Common.Null)
  , {-|
  The id used to register the request. The id can be used to deregister
  the request again. See also Registration#id.
  -}
  id :: (Maybe Data.Text.Text)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
  deriving Pretty via (ViaJSON DeclarationRegistrationOptions)

instance Aeson.ToJSON DeclarationRegistrationOptions where
  toJSON (DeclarationRegistrationOptions arg0 arg1 arg2) = Aeson.object $ concat $  ["workDoneProgress" Language.LSP.Protocol.Types.Common..=? arg0
    ,["documentSelector" Aeson..= arg1]
    ,"id" Language.LSP.Protocol.Types.Common..=? arg2]

instance Aeson.FromJSON DeclarationRegistrationOptions where
  parseJSON = Aeson.withObject "DeclarationRegistrationOptions" $ \arg -> DeclarationRegistrationOptions <$> arg Language.LSP.Protocol.Types.Common..:!? "workDoneProgress" <*> arg Aeson..: "documentSelector" <*> arg Language.LSP.Protocol.Types.Common..:!? "id"
