module JSONRPC.Utils where

import Data.Aeson

jsonOptions :: Options
jsonOptions = defaultOptions{omitNothingFields = True, fieldLabelModifier = modifier}
 where
  modifier :: String -> String
  modifier "data_" = "data"
  modifier xs = xs

jsonOptionsUntagged :: Options
jsonOptionsUntagged = jsonOptions{sumEncoding = UntaggedValue}
