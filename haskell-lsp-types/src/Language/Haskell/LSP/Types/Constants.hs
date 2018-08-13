
module Language.Haskell.LSP.Types.Constants where

import           Data.Aeson.TH

-- ---------------------------------------------------------------------

-- | Standard options for use when generating JSON instances
lspOptions :: Options
lspOptions = defaultOptions { omitNothingFields = True, fieldLabelModifier = drop 1 }
 -- NOTE: This needs to be in a separate file because of the TH stage restriction

customModifier :: String -> String
customModifier "_xdata" = "data"
customModifier "_xtype" = "type"
customModifier xs = drop 1 xs

