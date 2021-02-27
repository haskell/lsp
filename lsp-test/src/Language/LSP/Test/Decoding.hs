{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
module Language.LSP.Test.Decoding where

import           Prelude                 hiding ( id )
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Foldable
import           Data.Functor.Product
import           Data.Functor.Const
import           Control.Exception
import           Control.Lens
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Maybe
import           System.IO
import           System.IO.Error
import           Language.LSP.Types
import           Language.LSP.Types.Lens
import           Language.LSP.Test.Exceptions

import Data.IxMap
import Data.Kind

getAllMessages :: Handle -> IO [B.ByteString]
getAllMessages h = do
  done <- hIsEOF h
  if done
    then return []
    else do
      msg <- getNextMessage h

      (msg :) <$> getAllMessages h

-- | Fetches the next message bytes based on
-- the Content-Length header
getNextMessage :: Handle -> IO B.ByteString
getNextMessage h = do
  headers <- getHeaders h
  case read . init <$> lookup "Content-Length" headers of
    Nothing   -> throw NoContentLengthHeader
    Just size -> B.hGet h size

addHeader :: B.ByteString -> B.ByteString
addHeader content = B.concat
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
  where eofHandler e
          | isEOFError e = throw UnexpectedServerTermination
          | otherwise = throw e

type RequestMap = IxMap LspId (SMethod :: Method FromClient Request -> Type )

newRequestMap :: RequestMap
newRequestMap = emptyIxMap

updateRequestMap :: RequestMap -> LspId m -> SClientMethod m -> Maybe RequestMap
updateRequestMap reqMap id method = insertIxMap id method reqMap

getRequestMap :: [FromClientMessage] -> RequestMap
getRequestMap = foldl' helper emptyIxMap
 where
  helper :: RequestMap -> FromClientMessage -> RequestMap
  helper acc msg = case msg of
    FromClientMess m mess -> case splitClientMethod m of
      IsClientNot -> acc
      IsClientReq -> fromJust $ updateRequestMap acc (mess ^. id) m
      IsClientEither -> case mess of
        NotMess _ -> acc
        ReqMess msg -> fromJust $ updateRequestMap acc (msg ^. id) m
    _ -> acc

decodeFromServerMsg :: RequestMap -> B.ByteString -> (RequestMap, FromServerMessage)
decodeFromServerMsg reqMap bytes = unP $ fromJust $ parseMaybe p obj
  where obj = fromJust $ decode bytes :: Value
        p = parseServerMessage $ \lid ->
          let (mm, newMap) = pickFromIxMap lid reqMap
            in case mm of
              Nothing -> Nothing
              Just m -> Just $ (m, Pair m (Const newMap))
        unP (FromServerMess m msg) = (reqMap, FromServerMess m msg)
        unP (FromServerRsp (Pair m (Const newMap)) msg) = (newMap, FromServerRsp m msg)
        {-
        WorkspaceWorkspaceFolders      -> error "ReqWorkspaceFolders not supported yet"
        WorkspaceConfiguration         -> error "ReqWorkspaceConfiguration not supported yet"
        CustomServerMethod _
            | "id" `HM.member` obj && "method" `HM.member` obj -> ReqCustomServer $ fromJust $ decode bytes
            | "id" `HM.member` obj -> RspCustomServer $ fromJust $ decode bytes
            | otherwise -> NotCustomServer $ fromJust $ decode bytes

      Error e -> error e
      -}
