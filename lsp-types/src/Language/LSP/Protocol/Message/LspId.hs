{-# LANGUAGE TypeInType #-}
module Language.LSP.Protocol.Message.LspId where

import qualified Data.Aeson                         as A
import           Data.Hashable
import           Data.IxMap
import           Data.Text                          (Text)
import           GHC.Generics
import           Prettyprinter

import           Language.LSP.Protocol.Types.Common
import           Language.LSP.Protocol.Internal.Method
import           Language.LSP.Protocol.Message.Meta
import           Language.LSP.Protocol.Utils.Misc

-- | Id used for a request, Can be either a String or an Int
data LspId (m :: Method f Request) = IdInt !Int32 | IdString !Text
  deriving stock (Show,Read,Eq,Ord,Generic)

instance A.ToJSON (LspId m) where
  toJSON (IdInt i)    = A.toJSON i
  toJSON (IdString s) = A.toJSON s

instance A.FromJSON (LspId m) where
  parseJSON v@(A.Number _) = IdInt <$> A.parseJSON v
  parseJSON  (A.String  s) = return (IdString s)
  parseJSON _              = fail "LspId"

deriving via ViaJSON (LspId m) instance Pretty (LspId m)

instance IxOrd LspId where
  type Base LspId = SomeLspId
  toBase = SomeLspId

instance Hashable (LspId m) where
  hashWithSalt n (IdInt i)    = hashWithSalt n i
  hashWithSalt n (IdString t) = hashWithSalt n t

data SomeLspId where
  SomeLspId :: !(LspId m) -> SomeLspId

deriving stock instance Show SomeLspId
instance Eq SomeLspId where
  SomeLspId (IdInt a) == SomeLspId (IdInt b)       = a == b
  SomeLspId (IdString a) == SomeLspId (IdString b) = a == b
  _ == _                                           = False
instance Ord SomeLspId where
  compare (SomeLspId x) (SomeLspId y) = go x y
    where
      go (IdInt    a) (IdInt    b) = a `compare` b
      go (IdString a) (IdString b) = a `compare` b
      go (IdInt    _) (IdString _) = LT
      go (IdString _) (IdInt    _) = GT

instance Hashable SomeLspId where
  hashWithSalt n (SomeLspId lspId) = hashWithSalt n lspId
