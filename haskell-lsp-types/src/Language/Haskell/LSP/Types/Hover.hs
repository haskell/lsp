{-# LANGUAGE CPP              #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Language.Haskell.LSP.Types.Hover where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text                      ( Text )

import           Language.Haskell.LSP.Types.Common
import           Language.Haskell.LSP.Types.Location
import           Language.Haskell.LSP.Types.MarkupContent
import           Language.Haskell.LSP.Types.Progress
import           Language.Haskell.LSP.Types.TextDocument
import           Language.Haskell.LSP.Types.Utils


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

instance ToJSON MarkedString where
  toJSON (PlainString x) = toJSON x
  toJSON (CodeString  x) = toJSON x
instance FromJSON MarkedString where
  parseJSON (String t) = pure $ PlainString t
  parseJSON o            = CodeString <$> parseJSON o

-- -------------------------------------

data HoverContents =
    HoverContentsMS (List MarkedString)
  | HoverContents   MarkupContent
  deriving (Read,Show,Eq)

instance ToJSON HoverContents where
  toJSON (HoverContentsMS  x) = toJSON x
  toJSON (HoverContents    x) = toJSON x
instance FromJSON HoverContents where
  parseJSON v@(String _) = HoverContentsMS <$> parseJSON v
  parseJSON v@(Array _)  = HoverContentsMS <$> parseJSON v
  parseJSON v@(Object _) = HoverContents   <$> parseJSON v
                         <|> HoverContentsMS <$> parseJSON v
  parseJSON _ = mempty

-- -------------------------------------

#if __GLASGOW_HASKELL__ >= 804
instance Semigroup HoverContents where
  (<>) = mappend
#endif

instance Monoid HoverContents where
  mempty = HoverContentsMS (List [])

  HoverContents h1   `mappend` HoverContents         h2   = HoverContents (h1 `mappend` h2)
  HoverContents h1   `mappend` HoverContentsMS (List h2s) = HoverContents (mconcat (h1: (map toMarkupContent h2s)))
  HoverContentsMS (List h1s) `mappend` HoverContents         h2    = HoverContents (mconcat ((map toMarkupContent h1s) ++ [h2]))
  HoverContentsMS (List h1s) `mappend` HoverContentsMS (List h2s) = HoverContentsMS (List (h1s `mappend` h2s))

toMarkupContent :: MarkedString -> MarkupContent
toMarkupContent (PlainString s) = unmarkedUpContent s
toMarkupContent (CodeString (LanguageString lang s)) = markedUpContent lang s

-- -------------------------------------

data Hover =
  Hover
    { _contents :: HoverContents
    , _range    :: Maybe Range
    } deriving (Read,Show,Eq)

deriveJSON lspOptions ''Hover
