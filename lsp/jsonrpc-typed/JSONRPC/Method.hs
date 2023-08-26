module JSONRPC.Method where

import Data.Aeson
import Data.String (IsString)
import Data.Text
import Prettyprinter

newtype Method = Method {unMethod :: Text}
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, IsString, Pretty)
