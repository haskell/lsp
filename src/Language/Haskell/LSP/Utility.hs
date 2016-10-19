module Language.Haskell.LSP.Utility where


-- Based on Phoityne.VSCode.Utility

-- システム
import           Control.Exception (bracket)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ConfigFile as CFG
import           Data.Either.Utils
import qualified Data.List as L
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Tree as TR
import           Language.Haskell.LSP.Constant
import           System.IO
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

-- |
--  INI設定の指定したセクションに含まれる
--  すべて設定項目を返す。
--
getIniItems :: CFG.ConfigParser
            -> CFG.SectionSpec
            -> [(CFG.OptionSpec, String)]
getIniItems cp sec = foldr go [] opts
  where
    opts = forceEither $ CFG.options cp sec
    go opt acc = let value = forceEither $ CFG.get cp sec opt in
                 (opt, value) : acc


-- |
--
--
addChildTree :: TR.Tree a -> TR.Tree a -> TR.Tree a
addChildTree parent child = parent { TR.subForest = child : curForest}
  where curForest = TR.subForest parent


-- |
--
--
pushWithLimit :: (Eq a, Ord a) => [a] -> a -> Int -> [a]
pushWithLimit [] item _ = [item]
pushWithLimit buf item maxSize = if length buf > maxSize then item : L.init buf else item : buf

-- |
--
rdrop :: Eq a => Int -> [a] -> [a]
rdrop cnt = reverse . drop cnt . reverse

-- ---------------------------------------------------------------------

logs :: String -> IO ()
-- logs s = logm (B.pack s)
logs s = debugM _LOG_NAME s

logm :: B.ByteString -> IO ()
logm str = logs (lbs2str str)

-- | Append a 'ByteString' to a file.
appendFileAndFlush :: FilePath -> B.ByteString -> IO ()
appendFileAndFlush f txt = bracket (openBinaryFile f AppendMode) hClose
    (\hdl -> B.hPut hdl txt >> hFlush hdl)
