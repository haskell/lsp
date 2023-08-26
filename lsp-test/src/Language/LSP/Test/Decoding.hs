{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Language.LSP.Test.Decoding where

import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 qualified as B
import Data.Foldable
import Data.Maybe
import Language.LSP.Test.Exceptions
import Language.LSP.Server ()
import JSONRPC.Typed.Message
import JSONRPC.Method qualified as Untyped
import JSONRPC.Typed.Method (Role (..), SRole (..), toUntypedMethod)
import System.IO
import System.IO.Error
import Prelude hiding (id)
import Data.Map qualified as Map
import JSONRPC.Id
import qualified Language.LSP.Protocol.Message as LSP
import Data.Singletons

{- | Fetches the next message bytes based on
 the Content-Length header
-}
getNextMessage :: Handle -> IO B.ByteString
getNextMessage h = do
  headers <- getHeaders h
  case read . init <$> lookup "Content-Length" headers of
    Nothing -> throw NoContentLengthHeader
    Just size -> B.hGet h size

addHeader :: B.ByteString -> B.ByteString
addHeader content =
  B.concat
    [ "Content-Length: "
    , B.pack $ show $ B.length content
    , "\r\n"
    , "\r\n"
    , content
    ]

getHeaders :: Handle -> IO [(String, String)]
getHeaders h = do
  l <- catch (hGetLine h) eofHandler
  let (name, val) = span (/= ':') l
  if null val then return [] else ((name, drop 2 val) :) <$> getHeaders h
 where
  eofHandler e
    | isEOFError e = throw UnexpectedServerTermination
    | otherwise = throw e

type RequestMap = Map.Map Id Untyped.Method

newRequestMap :: RequestMap
newRequestMap = mempty

updateRequestMap :: RequestMap -> Id -> LSP.Method -> RequestMap
updateRequestMap reqMap id method = Map.insert id (toUntypedMethod method) reqMap

getRequestMap :: [SomeMessage Server LSP.Method] -> RequestMap
getRequestMap = foldl' helper mempty
 where
  helper :: RequestMap -> SomeMessage Server LSP.Method -> RequestMap
  helper acc (SomeMessage msg) = case msg of
    Not _ _ -> acc
    Rsp _ _ -> acc
    Req m msg -> updateRequestMap acc (msg.id) (fromSing m)

decodeFromServerMsg :: RequestMap -> B.ByteString -> (RequestMap, SomeMessage Server LSP.Method)
decodeFromServerMsg reqMap bytes = unP $ parse p obj
 where
  obj = fromJust $ decode bytes :: Value
  p = withSingI SServer $ parseSomeMessage (\m -> Map.lookup m reqMap)
  unP (Success m@(SomeMessage (Rsp _ msg))) = (Map.delete (msg.id) reqMap, m)
  unP (Success m) = (reqMap, m)
  unP (Error e) = error $ "Error decoding " <> show obj <> " :" <> e

{-
  WorkspaceWorkspaceFolders      -> error "ReqWorkspaceFolders not supported yet"
  WorkspaceConfiguration         -> error "ReqWorkspaceConfiguration not supported yet"
  CustomServerMethod _
      | "id" `HM.member` obj && "method" `HM.member` obj -> ReqCustomServer $ fromJust $ decode bytes
      | "id" `HM.member` obj -> RspCustomServer $ fromJust $ decode bytes
      | otherwise -> NotCustomServer $ fromJust $ decode bytes

Error e -> error e
-}
