{-# LANGUAGE FlexibleContexts #-}
module Language.Haskell.LSP.Test.Files
  ( loadSwappedFiles
  , FileMap
  , emptyFileMap
  )
where

import           Language.Haskell.LSP.Core
import qualified Language.Haskell.LSP.Control  as Control
import           Language.Haskell.LSP.Types        hiding ( error )
import           Data.Default
import           Control.Lens
import           Control.Monad
import           Control.Concurrent
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Map                      as Map
import           Data.Maybe
import           System.Directory
import           System.IO

type FileMap = Map.Map FilePath FilePath

emptyFileMap :: FileMap
emptyFileMap = Map.empty

buildFiles
  :: (HasParams a b, HasTextDocument b c, HasUri c Uri)
  => [a]
  -> FileMap
  -> IO FileMap
buildFiles ns oldMap = foldM createFile oldMap ns
 where
  createFile map n = do
    let fp = fromMaybe (error "Couldn't convert file path")
                       (uriToFilePath $ n ^. params . textDocument . uri)
    if Map.member fp map
      then return map
      else do
        tmpDir        <- getTemporaryDirectory
        (tmpFp, tmpH) <- openTempFile tmpDir "lspTestDoc"
        readFile fp >>= hPutStr tmpH
        return $ Map.insert fp tmpFp map

swapFile :: (HasUri a Uri) => FileMap -> a -> a
swapFile m msg = fromMaybe msg $ do
  let oldUri = msg ^. uri
  oldFp <- uriToFilePath oldUri
  newFp <- Map.lookup oldFp m
  let newUri = filePathToUri newFp
  return $ uri .~ newUri $ msg

loadSwappedFiles :: FileMap -> Handle -> IO ([B.ByteString], FileMap)
loadSwappedFiles map h = do
  fileMapVar <- newMVar map
  msgsVar    <- newMVar []
  nullH      <- openFile "/dev/null" WriteMode
  Control.runWithHandles h
                         nullH
                         (const $ Right (), const $ return Nothing)
                         (handlers msgsVar fileMapVar)
                         def
                         Nothing
                         Nothing
  newMap <- readMVar fileMapVar
  msgs   <- reverse <$> readMVar msgsVar
  return (msgs, newMap)

handlers :: MVar [B.ByteString] -> MVar FileMap -> Handlers
handlers msgs fileMap = Handlers
  {
    -- Requests
    hoverHandler                             = Just put
  , completionHandler                        = Just put
  , completionResolveHandler                 = Just put
  , signatureHelpHandler                     = Just put
  , definitionHandler                        = Just put
  , referencesHandler                        = Just put
  , documentHighlightHandler                 = Just put
  , documentSymbolHandler                    = Just $ swapUri (params . textDocument)
  , workspaceSymbolHandler                   = Just put
  , codeActionHandler                        = Just put
  , codeLensHandler                          = Just put
  , codeLensResolveHandler                   = Just put
  , documentFormattingHandler                = Just put
  , documentRangeFormattingHandler           = Just put
  , documentTypeFormattingHandler            = Just put
  , renameHandler                            = Just $ swapUri (params . textDocument)
  , documentLinkHandler                      = Just $ swapUri (params . textDocument)
  , documentLinkResolveHandler               = Just put
  , executeCommandHandler                    = Just put
  , initializeRequestHandler                 = Just put
    -- Notifications
  , didChangeConfigurationParamsHandler      = Just put
  , didOpenTextDocumentNotificationHandler   = Just $ swapUri (params . textDocument)
  , didChangeTextDocumentNotificationHandler = Just $ swapUri (params . textDocument)
  , didCloseTextDocumentNotificationHandler  = Just $ swapUri (params . textDocument)
  , didSaveTextDocumentNotificationHandler   = Just $ swapUri (params . textDocument)
  , willSaveWaitUntilTextDocHandler          = Just put
  , didChangeWatchedFilesNotificationHandler = Just put
  , initializedHandler                       = Just put
  , willSaveTextDocumentNotificationHandler  = Just $ swapUri (params . textDocument)
  , cancelNotificationHandler                = Just put
  , exitNotificationHandler                  = Just put
    -- Responses
  , responseHandler                          = Just put
  }
 where
  swapUri f msg = do
    modifyMVar_ fileMap (buildFiles [msg])
    map <- readMVar fileMap
    put $ swapFile map $ msg ^. f

  put :: ToJSON a => a -> IO ()
  put msg = modifyMVar_ msgs (return . (encode msg :))
