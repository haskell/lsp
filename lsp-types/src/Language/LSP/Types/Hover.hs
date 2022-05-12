{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Language.LSP.Types.Hover where

import           Data.Aeson.TH
import           Data.Text                      ( Text )

import           Language.LSP.Types.Common
import           Language.LSP.Types.Location
import           Language.LSP.Types.MarkupContent
import           Language.LSP.Types.Progress
import           Language.LSP.Types.TextDocument
import           Language.LSP.Types.Utils


-- -------------------------------------

data HoverClientCapabilities =
  HoverClientCapabilities
    { _dynamicRegistration :: Maybe Bool
    , _contentFormat :: Maybe (List MarkupKind)
    } deriving (Show, Read, Eq)
deriveJSON lspOptions ''HoverClientCapabilities

makeExtendingDatatype "HoverOptions" [''WorkDoneProgressOptions] []
deriveJSON lspOptions ''HoverOptions

makeExtendingDatatype "HoverRegistrationOptions" [''TextDocumentRegistrationOptions, ''HoverOptions] []
deriveJSON lspOptions ''HoverRegistrationOptions

makeExtendingDatatype "HoverParams" [''TextDocumentPositionParams, ''WorkDoneProgressParams] []
deriveJSON lspOptions ''HoverParams

-- -------------------------------------

data LanguageString =
  LanguageString
    { _language :: Text
    , _value    :: Text
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''LanguageString

{-# DEPRECATED MarkedString, PlainString, CodeString "Use MarkupContent instead, since 3.3.0 (11/24/2017)" #-}
data MarkedString =
    PlainString Text
  | CodeString LanguageString
    deriving (Eq,Read,Show)

deriveJSON lspOptionsUntagged ''MarkedString

-- -------------------------------------

toMarkupContent :: MarkedString -> MarkupContent
toMarkupContent (PlainString s) = unmarkedUpContent s
toMarkupContent (CodeString (LanguageString lang s)) = markedUpContent lang s

-- -------------------------------------

data Hover =
  Hover
    { _contents :: MarkedString |? List MarkedString |? MarkupContent
    , _range    :: Maybe Range
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''Hover
