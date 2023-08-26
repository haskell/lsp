module JSONRPC.Id where

import Data.Aeson qualified as A
import Data.Hashable
import Data.Int (Int32)
import Data.Text
import GHC.Generics
import Prettyprinter

-- | Id used for a request, Can be either a String or an Int
data Id = IdInt !Int32 | IdString !Text
  deriving stock (Show, Read, Eq, Ord, Generic)

instance Pretty Id where
  pretty (IdInt i) = pretty i
  pretty (IdString s) = pretty s

instance A.ToJSON Id where
  toJSON (IdInt i) = A.toJSON i
  toJSON (IdString s) = A.toJSON s

instance A.FromJSON Id where
  parseJSON v@(A.Number _) = IdInt <$> A.parseJSON v
  parseJSON (A.String s) = return (IdString s)
  parseJSON _ = fail "An Id must be either a number or a string"

instance Hashable Id where
  hashWithSalt n (IdInt i) = hashWithSalt n i
  hashWithSalt n (IdString t) = hashWithSalt n t
