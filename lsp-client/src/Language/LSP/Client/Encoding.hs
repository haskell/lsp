module Language.LSP.Client.Encoding where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Prelude

addHeader :: LazyByteString -> LazyByteString
addHeader content =
    mconcat
        [ "Content-Length: "
        , LazyByteString.pack $ show $ LazyByteString.length content
        , "\r\n"
        , "\r\n"
        , content
        ]

encode :: ToJSON a => a -> LazyByteString
encode = addHeader . Aeson.encode
