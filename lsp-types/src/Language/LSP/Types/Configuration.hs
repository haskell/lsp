{-# LANGUAGE TemplateHaskell #-}
module Language.LSP.Types.Configuration where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Language.LSP.Types.Common
import Language.LSP.Types.Utils

-- -------------------------------------

data DidChangeConfigurationClientCapabilities =
  DidChangeConfigurationClientCapabilities
    { _dynamicRegistration :: Maybe Bool -- ^Did change configuration
                                         -- notification supports dynamic
                                         -- registration.
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DidChangeConfigurationClientCapabilities

data DidChangeConfigurationParams =
  DidChangeConfigurationParams
  { _settings :: Value -- ^ The actual changed settings
  } deriving (Show, Read, Eq)

deriveJSON lspOptions ''DidChangeConfigurationParams

-- ---------------------------------------------------------------------

data ConfigurationItem =
  ConfigurationItem
    { _scopeUri :: Maybe Text -- ^ The scope to get the configuration section for.
    , _section  :: Maybe Text -- ^ The configuration section asked for.
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''ConfigurationItem

data ConfigurationParams =
  ConfigurationParams
    { _items :: List ConfigurationItem
    } deriving (Show, Read, Eq)
deriveJSON lspOptions ''ConfigurationParams
