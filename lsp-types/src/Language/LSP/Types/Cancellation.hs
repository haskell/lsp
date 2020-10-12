{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
module Language.LSP.Types.Cancellation where
  
import Data.Aeson.TH
import Language.LSP.Types.LspId
import Language.LSP.Types.Utils

data CancelParams = forall m.
  CancelParams
    { -- | The request id to cancel.
      _id :: LspId m
    }

deriving instance Read CancelParams
deriving instance Show CancelParams
instance Eq CancelParams where
  (CancelParams a) == CancelParams b =
    case (a,b) of
      (IdInt x, IdInt y) -> x == y
      (IdString x, IdString y) -> x == y
      _ -> False

deriveJSON lspOptions ''CancelParams
