{-# LANGUAGE CPP              #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Language.Haskell.LSP.Types.Hover where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text                      ( Text )
import           Language.Haskell.LSP.Types.Constants
import           Language.Haskell.LSP.Types.List
import           Language.Haskell.LSP.Types.Location
import           Language.Haskell.LSP.Types.MarkupContent
import           Language.Haskell.LSP.Types.Progress
import           Language.Haskell.LSP.Types.TextDocument
import           Language.Haskell.LSP.Types.Utils

-- ---------------------------------------------------------------------

{-
/**
 * MarkedString can be used to render human readable text. It is either a markdown string
 * or a code-block that provides a language and a code snippet. The language identifier
 * is semantically equal to the optional language identifier in fenced code blocks in GitHub
 * issues. See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
 *
 * The pair of a language and a value is an equivalent to markdown:
 * ```${language}
 * ${value}
 * ```
 *
 * Note that markdown strings will be sanitized - that means html will be escaped.
* @deprecated use MarkupContent instead.
*/
type MarkedString = string | { language: string; value: string };

    error: code and message set in case an exception happens during the hover
    request.

Registration Options: TextDocumentRegistrationOptions

-}

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

-- ---------------------------------------------------------------------
{-
Hover Request

The hover request is sent from the client to the server to request hover
information at a given text document position.

    Changed: In 2.0 the request uses TextDocumentPositionParams with a proper
    textDocument and position property. In 1.0 the uri of the referenced text
    document was inlined into the params object.

Request

    method: 'textDocument/hover'
    params: TextDocumentPositionParams

Response

    result: Hover | null defined as follows:


/**
 * The result of a hover request.
 */
interface Hover {
        /**
         * The hover's content
         */
        contents: MarkedString | MarkedString[] | MarkupContent;

        /**
         * An optional range is a range inside a text document
         * that is used to visualize a hover, e.g. by changing the background color.
         */
        range?: Range;
}

-}


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

data HoverOptions =
  HoverOptions
    { _workDoneProgressOptions :: WorkDoneProgressOptions
    } deriving (Read,Show,Eq)

deriveJSONExtendFields lspOptions ''HoverOptions ["_workDoneProgressOptions"]

data HoverRegistrationOptions =
  HoverRegistrationOptions
    { _textDocumentRegistrationOptions :: TextDocumentRegistrationOptions
    , _hoverOptions     :: HoverOptions
    } deriving (Read,Show,Eq)

deriveJSONExtendFields lspOptions ''HoverRegistrationOptions ["_textDocumentRegistrationOptions", "_hoverOptions"]
