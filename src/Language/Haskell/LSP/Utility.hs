module Language.Haskell.LSP.Utility where


-- Based on Phoityne.VSCode.Utility

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Language.Haskell.LSP.Constant
import           System.Log.Logger

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
-- ---------------------------------------------------------------------

-- |
--  UTF8文字列をByteStringへの変換
--
str2bs :: String -> BS.ByteString
str2bs = TE.encodeUtf8 . T.pack

-- |
--  ByteStringをUTF8文字列への変換
--
bs2str :: BS.ByteString -> String
bs2str = T.unpack. TE.decodeUtf8

-- |
--  UTF8文字列をLazyByteStringへの変換
--
str2lbs :: String -> LBS.ByteString
str2lbs = TLE.encodeUtf8 . TL.pack

-- |
--  LazyByteStringをUTF8文字列への変換
--
lbs2str :: LBS.ByteString -> String
lbs2str = TL.unpack. TLE.decodeUtf8


-- ---------------------------------------------------------------------

logs :: String -> IO ()
logs s = debugM _LOG_NAME s

logm :: B.ByteString -> IO ()
logm str = logs (lbs2str str)

