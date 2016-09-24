{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}

module Language.Haskell.LSP.Core (
  handleRequest
, DebugContextData(..)
, defaultDebugContextData
, initializeRequestHandler
) where

import Language.Haskell.LSP.Constant
import Language.Haskell.LSP.Utility
-- import Phoityne.VSCode.IO.Utility
-- import qualified Phoityne.VSCode.TH.BreakpointJSON as J
-- import qualified Phoityne.VSCode.TH.CompletionsItemJSON as J
-- import qualified Phoityne.VSCode.TH.CompletionsArgumentsJSON as J
-- import qualified Phoityne.VSCode.TH.CompletionsResponseBodyJSON as J
-- import qualified Phoityne.VSCode.TH.CompletionsRequestJSON as J
-- import qualified Phoityne.VSCode.TH.CompletionsResponseJSON as J
-- import qualified Phoityne.VSCode.TH.ConfigurationDoneRequestJSON as J
-- import qualified Phoityne.VSCode.TH.ConfigurationDoneResponseJSON as J
-- import qualified Phoityne.VSCode.TH.ContinueRequestJSON as J
-- import qualified Phoityne.VSCode.TH.ContinueResponseJSON as J
-- import qualified Phoityne.VSCode.TH.DisconnectRequestJSON as J
-- import qualified Phoityne.VSCode.TH.DisconnectResponseJSON as J
-- import qualified Phoityne.VSCode.TH.EvaluateArgumentsJSON as J
-- import qualified Phoityne.VSCode.TH.EvaluateBodyJSON as J
-- import qualified Phoityne.VSCode.TH.EvaluateRequestJSON as J
-- import qualified Phoityne.VSCode.TH.EvaluateResponseJSON as J
-- import qualified Phoityne.VSCode.TH.InitializedEventJSON as J
import qualified Language.Haskell.LSP.TH.InitializeRequestJSON as J
import qualified Language.Haskell.LSP.TH.InitializeResponseCapabilitiesJSON as J
import qualified Language.Haskell.LSP.TH.InitializeResponseJSON as J
-- import qualified Phoityne.VSCode.TH.LaunchRequestArgumentsJSON as J
-- import qualified Phoityne.VSCode.TH.LaunchRequestJSON as J
-- import qualified Phoityne.VSCode.TH.LaunchResponseJSON as J
-- import qualified Phoityne.VSCode.TH.NextRequestJSON as J
-- import qualified Phoityne.VSCode.TH.NextResponseJSON as J
import qualified Language.Haskell.LSP.TH.OutputEventJSON as J
import qualified Language.Haskell.LSP.TH.OutputEventBodyJSON as J
-- import qualified Phoityne.VSCode.TH.PauseRequestJSON as J
-- import qualified Phoityne.VSCode.TH.PauseResponseJSON as J
import qualified Language.Haskell.LSP.TH.RequestJSON as J
-- import qualified Phoityne.VSCode.TH.ScopesArgumentsJSON as J
-- import qualified Phoityne.VSCode.TH.ScopesRequestJSON as J
-- import qualified Phoityne.VSCode.TH.ScopesResponseJSON as J
-- import qualified Phoityne.VSCode.TH.SetBreakpointsRequestArgumentsJSON as J
-- import qualified Phoityne.VSCode.TH.SetBreakpointsRequestJSON as J
-- import qualified Phoityne.VSCode.TH.SetBreakpointsResponseBodyJSON as J
-- import qualified Phoityne.VSCode.TH.SetBreakpointsResponseJSON as J
-- import qualified Phoityne.VSCode.TH.SetFunctionBreakpointsRequestArgumentsJSON as J
-- import qualified Phoityne.VSCode.TH.SetFunctionBreakpointsRequestJSON as J
-- import qualified Phoityne.VSCode.TH.SetFunctionBreakpointsResponseBodyJSON as J
-- import qualified Phoityne.VSCode.TH.SetFunctionBreakpointsResponseJSON as J
-- import qualified Phoityne.VSCode.TH.SourceBreakpointJSON as J
-- import qualified Phoityne.VSCode.TH.FunctionBreakpointJSON as J
-- import qualified Phoityne.VSCode.TH.SourceJSON as J
-- import qualified Phoityne.VSCode.TH.SourceRequestJSON as J
-- import qualified Phoityne.VSCode.TH.SourceResponseJSON as J
-- import qualified Phoityne.VSCode.TH.StackFrameJSON as J
-- import qualified Phoityne.VSCode.TH.StackTraceBodyJSON as J
-- import qualified Phoityne.VSCode.TH.StackTraceRequestJSON as J
-- import qualified Phoityne.VSCode.TH.StackTraceResponseJSON as J
-- import qualified Phoityne.VSCode.TH.StepInRequestJSON as J
-- import qualified Phoityne.VSCode.TH.StepInResponseJSON as J
-- import qualified Phoityne.VSCode.TH.StepOutRequestJSON as J
-- import qualified Phoityne.VSCode.TH.StepOutResponseJSON as J
-- import qualified Phoityne.VSCode.TH.StoppedEventJSON as J
import qualified Language.Haskell.LSP.TH.TerminatedEventJSON as J
import qualified Language.Haskell.LSP.TH.TerminatedEventBodyJSON as J
-- import qualified Phoityne.VSCode.TH.ThreadsRequestJSON as J
-- import qualified Phoityne.VSCode.TH.ThreadsResponseJSON as J
-- import qualified Phoityne.VSCode.TH.VariableJSON as J
-- import qualified Phoityne.VSCode.TH.VariablesBodyJSON as J
-- import qualified Phoityne.VSCode.TH.VariablesRequestJSON as J
-- import qualified Phoityne.VSCode.TH.VariablesResponseJSON as J

-- import qualified Phoityne.GHCi as G

import System.IO
import System.FilePath
import System.Directory
import System.Log.Logger
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.String.Utils as U
import qualified Data.List as L
import qualified Control.Exception as E
import qualified Data.Map as MAP
import Control.Concurrent
import Data.List.Split
import Data.Char
import Control.Monad
import qualified System.FSNotify as FSN
import qualified System.Log.Logger as L
import qualified System.Log.Formatter as L
import qualified System.Log.Handler as LH
import qualified System.Log.Handler.Simple as LHS
import Safe
import Text.Parsec

-- |
--
--
data DebugContextData =
  DebugContextData {
    resSeqDebugContextData                  :: Int
  -- , functionBreakPointDatasDebugContextData :: BreakPointDatas
  -- , breakPointDatasDebugContextData         :: BreakPointDatas
  -- , workspaceDebugContextData               :: FilePath
  -- , startupDebugContextData                 :: FilePath
  -- , debugStartedDebugContextData            :: Bool
  -- , debugStoppedPosDebugContextData         :: Maybe G.SourcePosition
  -- , currentFrameIdDebugContextData          :: Int
  -- , modifiedDebugContextData                :: Bool
  -- , ghciProcessDebugContextData             :: Maybe G.GHCiProcess
  , responseHandlerDebugContextData         :: BSL.ByteString -> IO ()
  }


-- |
--
--
data BreakPointData =
  BreakPointData {
    nameBreakPointData :: String
  , filePathBreakPointData   :: FilePath
  , lineNoBreakPointData     :: Int
  , breakNoBreakPointData    :: Maybe Int
  , conditionBreakPointData  :: Maybe String
  } deriving (Show, Read, Eq, Ord)


-- |
--
--
type BreakPointDataKey = (FilePath, Int)

-- |
--
--
type BreakPointDatas = MAP.Map BreakPointDataKey BreakPointData


-- |
--
--
_INITIAL_RESPONSE_SEQUENCE :: Int
_INITIAL_RESPONSE_SEQUENCE = 0


-- |
--
--
_TWO_CRLF :: String
_TWO_CRLF = "\r\n\r\n"

-- |
--
--
_SEP_WIN :: Char
_SEP_WIN = '\\'

-- |
--
--
_SEP_UNIX :: Char
_SEP_UNIX = '/'

{-
-- |
--
--
_TASKS_JSON_FILE_CONTENTS :: BSL.ByteString
_TASKS_JSON_FILE_CONTENTS = str2lbs $ U.join "\n" $
  [
    "{"
  , "  // atuomatically created by phoityne-vscode"
  , "  "
  , "  \"version\": \"0.1.0\","
  , "  \"isShellCommand\": true,"
  , "  \"showOutput\": \"always\","
  , "  \"suppressTaskName\": true,"
  , "  \"windows\": {"
  , "    \"command\": \"cmd\","
  , "    \"args\": [\"/c\"]"
  , "  },"
  , "  \"linux\": {"
  , "    \"command\": \"sh\","
  , "    \"args\": [\"-c\"]"
  , "  },"
  , "  \"osx\": {"
  , "    \"command\": \"sh\","
  , "    \"args\": [\"-c\"]"
  , "  },"
  , "  \"tasks\": ["
  , "    {"
  , "       \"taskName\": \"stack build\","
  , "       \"args\": [ \"echo START_STACK_BUILD && cd ${workspaceRoot} && stack build && echo END_STACK_BUILD \" ]"
  , "    },"
  , "    { "
  , "       \"isBuildCommand\": true,"
  , "       \"taskName\": \"stack clean & build\","
  , "       \"args\": [ \"echo START_STACK_CLEAN_AND_BUILD && cd ${workspaceRoot} && stack clean && stack build && echo END_STACK_CLEAN_AND_BUILD \" ]"
  , "    },"
  , "    { "
  , "       \"isTestCommand\": true,"
  , "       \"taskName\": \"stack test\","
  , "       \"args\": [ \"echo START_STACK_TEST && cd ${workspaceRoot} && stack test && echo END_STACK_TEST \" ]"
  , "    },"
  , "    { "
  , "       \"isWatching\": true,"
  , "       \"taskName\": \"stack watch\","
  , "       \"args\": [ \"echo START_STACK_WATCH && cd ${workspaceRoot} && stack build --test --no-run-tests --file-watch && echo END_STACK_WATCH \" ]"
  , "    }"
  , "  ]"
  , "}"
  ]

-}
-- |
--
--
_ERR_MSG_URL :: [String]
_ERR_MSG_URL = [ "`stack update` and install new phoityen-vscode."
               , "Or check information on https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode"
               ]


-- |
--
--
defaultDebugContextData :: DebugContextData
-- defaultDebugContextData = DebugContextData _INITIAL_RESPONSE_SEQUENCE (MAP.fromList []) (MAP.fromList []) "" "" False Nothing 0 False Nothing BSL.putStr
defaultDebugContextData = DebugContextData _INITIAL_RESPONSE_SEQUENCE BSL.putStr

-- |
--
--
-- getKeyOfSourcePosition :: G.SourcePosition -> BreakPointDataKey
-- getKeyOfSourcePosition (G.SourcePosition file line _ _ _) = (file, line)


-- |
--
--
handleRequest :: MVar DebugContextData -> BSL.ByteString -> BSL.ByteString -> IO ()
handleRequest mvarDat contLenStr jsonStr = do
  return ()
  case J.eitherDecode jsonStr :: Either String J.Request of
    Left  err -> do
      -- req_secが不明のため、エラー出力のみ行う。
      -- ただし、initializeが完了していない場合は、エラー出力イベントが受理されない。
      -- launchしていな場合はログ出力ができない。
      -- 無視して、次のリクエストを待つ。
      let msg =  L.intercalate " " [ "request request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
              ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL)
              ++ "\n"

      sendErrorEvent mvarDat msg

      resSeq <- getIncreasedResponseSequence mvarDat
      let terminatedEvt    = J.defaultTerminatedEvent resSeq
          terminatedEvtStr = J.encode terminatedEvt
      sendEventL terminatedEvtStr

    Right (J.Request cmd) -> handle contLenStr jsonStr cmd

  where

    handle contLenStr jsonStr "initialize" = case J.eitherDecode jsonStr :: Either String J.InitializeRequest of
      Right req -> initializeRequestHandler mvarDat req
      Left  err -> do
        -- initializeが完了していない場合は、エラー出力イベントが受理されない。
        -- responceをエラーで返す。メッセージは1行で作成する必要がある。
        -- launchしていな場合はログ出力ができない。
        -- res_seqは1固定とする。
        let msg = L.intercalate " " $ ["initialize request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err] ++ _ERR_MSG_URL
        resSeq <- getIncreasedResponseSequence mvarDat
        sendResponse $ J.encode $ J.parseErrorInitializeResponse resSeq msg

{-
    handle contLenStr jsonStr "launch" = case J.eitherDecode jsonStr :: Either String J.LaunchRequest of
      Right req -> launchRequestHandler mvarDat req
      Left  err -> do
        -- launchしていな場合はログ出力ができない。
        -- req_secが不明のため、エラー出力のみ行う。
        let msg = L.intercalate " " ["launch request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent mvarDat msg

        resSeq <- getIncreasedResponseSequence mvarDat
        let terminatedEvt    = J.defaultTerminatedEvent resSeq
            terminatedEvtStr = J.encode terminatedEvt
        sendEventL terminatedEvtStr


    handle contLenStr jsonStr "configurationDone" = case J.eitherDecode jsonStr :: Either String J.ConfigurationDoneRequest of
      Right req -> configurationDoneRequestHandler mvarDat req
      Left  err -> do
        let msg = L.intercalate " " ["configurationDone request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent mvarDat msg

    handle contLenStr jsonStr "disconnect" = case J.eitherDecode jsonStr :: Either String J.DisconnectRequest of
      Right req -> disconnectRequestHandler mvarDat req
      Left  err -> do
        let msg = L.intercalate " " ["disconnect request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent mvarDat msg


    handle contLenStr jsonStr "setBreakpoints" = case J.eitherDecode jsonStr :: Either String J.SetBreakpointsRequest of
      Right req -> setBreakpointsRequestHandler mvarDat req
      Left  err -> do
        let msg = L.intercalate " " ["setBreakpoints request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent mvarDat msg


    handle contLenStr jsonStr "setFunctionBreakpoints" = case J.eitherDecode jsonStr :: Either String J.SetFunctionBreakpointsRequest of
      Right req -> setFunctionBreakpointsRequestHandler mvarDat req
      Left  err -> do
        let msg = L.intercalate " " ["setFunctionBreakpoints request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent mvarDat msg


    handle contLenStr jsonStr "continue" = case J.eitherDecode jsonStr :: Either String J.ContinueRequest of
      Right req -> continueRequestHandler mvarDat req
      Left  err -> do
        let msg = L.intercalate " " ["continue request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent mvarDat msg


    handle contLenStr jsonStr "next" = case J.eitherDecode jsonStr :: Either String J.NextRequest of
      Right req -> nextRequestHandler mvarDat req
      Left  err -> do
        let msg = L.intercalate " " ["next request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent mvarDat msg


    handle contLenStr jsonStr "stepIn" = case J.eitherDecode jsonStr :: Either String J.StepInRequest of
      Right req -> stepInRequestHandler mvarDat req
      Left  err -> do
        let msg = L.intercalate " " ["stepIn request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent mvarDat msg

    -- |
    --  not supported.
    --
    handle contLenStr jsonStr "stepOut" = case J.eitherDecode jsonStr :: Either String J.StepOutRequest of
      Right req -> do
        resSeq <- getIncreasedResponseSequence mvarDat
        let res    = J.defaultStepOutResponse resSeq req
            resStr = J.encode $ res{J.successStepOutResponse = False, J.messageStepOutResponse = "unsupported command."}
        sendResponse resStr

        sendErrorEvent mvarDat "stepOut command is not supported."

      Left  err -> do
        let msg = L.intercalate " " ["stepOut request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent mvarDat msg

    -- |
    --  not supported.
    --
    handle contLenStr jsonStr "pause" = case J.eitherDecode jsonStr :: Either String J.PauseRequest of
      Right req -> do
        resSeq <- getIncreasedResponseSequence mvarDat
        let res    = J.defaultPauseResponse resSeq req
            resStr = J.encode $ res{J.successPauseResponse = False, J.messagePauseResponse = "unsupported command."}
        sendResponse resStr

        sendErrorEvent  mvarDat "pause command is not supported."

      Left  err -> do
        let msg = L.intercalate " " ["pause request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent  mvarDat msg


    handle contLenStr jsonStr "stackTrace" = case J.eitherDecode jsonStr :: Either String J.StackTraceRequest of
      Right req -> stackTraceRequestHandler mvarDat req
      Left  err -> do
        let msg = L.intercalate " " ["stackTrace request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent  mvarDat msg


    handle contLenStr jsonStr "scopes" = case J.eitherDecode jsonStr :: Either String J.ScopesRequest of
      Right req -> scopesRequestHandler mvarDat req
      Left  err -> do
        let msg = L.intercalate " " ["scopes request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent mvarDat msg


    handle contLenStr jsonStr "variables" = case J.eitherDecode jsonStr :: Either String J.VariablesRequest of
      Right req -> variablesRequestHandler mvarDat req
      Left  err -> do
        let msg = L.intercalate " " ["variables request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent mvarDat msg

    -- |
    --  not supported.
    --
    handle contLenStr jsonStr "source" = case J.eitherDecode jsonStr :: Either String J.SourceRequest of
      Right req -> do
        resSeq <- getIncreasedResponseSequence mvarDat
        let res    = J.defaultSourceResponse resSeq req
            resStr = J.encode $ res{J.successSourceResponse = False, J.messageSourceResponse = "unsupported command."}
        sendResponse resStr

        sendErrorEvent  mvarDat "source command is not supported."

      Left  err -> do
        let msg = L.intercalate " " ["source request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent mvarDat msg


    handle contLenStr jsonStr "threads" = case J.eitherDecode jsonStr :: Either String J.ThreadsRequest of
      Right req -> threadsRequestHandler mvarDat req
      Left  err -> do
        let msg = L.intercalate " " ["threads request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent mvarDat msg


    handle contLenStr jsonStr "evaluate" = case J.eitherDecode jsonStr :: Either String J.EvaluateRequest of
      Right req -> evaluateRequestHandler mvarDat req
      Left  err -> do
        let msg = L.intercalate " " ["evaluate request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent mvarDat msg

    handle contLenStr jsonStr "completions" = case J.eitherDecode jsonStr :: Either String J.CompletionsRequest of
      Right req -> completionsRequestHandler mvarDat req
      Left  err -> do
        let msg = L.intercalate " " ["completions request parse error.", lbs2str contLenStr, lbs2str jsonStr, show err]
                ++ L.intercalate "\n" ("" : "" : _ERR_MSG_URL) ++ "\n"
        sendErrorEvent mvarDat msg

-}
    handle contLenStr jsonStr cmd = do
      let msg = L.intercalate " " ["unknown request command.", cmd, lbs2str contLenStr, lbs2str jsonStr]
      sendErrorEvent mvarDat msg

-- |
--
sendEvent :: BSL.ByteString -> IO ()
sendEvent str = sendResponseInternal str

-- |
--
sendEventL :: BSL.ByteString -> IO ()
sendEventL str = do
  infoM _LOG_NAME $ "[EVENT]" ++ lbs2str str
  sendEvent str


-- |
--
sendResponseL :: BSL.ByteString -> IO ()
sendResponseL str = do
  infoM _LOG_NAME $ "[RESPONSE]" ++ lbs2str str
  sendResponse str


-- |
--
sendResponse :: BSL.ByteString -> IO ()
sendResponse str = sendResponseInternal str

-- |
--
sendResponse2 :: MVar DebugContextData -> BSL.ByteString -> IO ()
sendResponse2 mvarCtx str = do
  ctx <- readMVar mvarCtx
  responseHandlerDebugContextData ctx str
    -- "{\"seq\":1,\"type\":\"response\",\"request_seq\":1,\"success\":true,\"command\":\"initialize\",\"message\":\"\",\"body\":{\"supportsCompletionsRequest\":true}}"
    --  responseHandlerDebugContextData ctx $ "{\"seq\":1,\"type\":\"response\",\"request_seq\":1,\"success\":true,\"command\":\"initialize\",\"message\":\"\",\"body\":{\"supportsConfigurationDoneRequest\":true,\"supportsFunctionBreakpoints\":false,\"supportsConditionalBreakpoints\":true,\"supportsEvaluateForHovers\":true,\"exceptionBreakpointFilters\":[],\"supportsStepBack\":false,\"supportsSetVariable\":false,\"supportsRestartFrame\":false,\"supportsGotoTargetsRequest\":false,\"supportsStepInTargetsRequest\":false,\"supportsCompletionsRequest\":false}}"

-- |
--
sendResponseInternal :: BSL.ByteString -> IO ()
sendResponseInternal str = do
  BSL.hPut stdout $ BSL.append "Content-Length: " $ str2lbs $ show (BSL.length str)
  BSL.hPut stdout $ str2lbs _TWO_CRLF
  BSL.hPut stdout str
  hFlush stdout

{-
-- |
--
--
sendConsoleEvent :: MVar DebugContextData -> String -> IO ()
sendConsoleEvent mvarCtx msg = do
  resSeq <- getIncreasedResponseSequence mvarCtx
  let outEvt  = J.defaultOutputEvent resSeq
      outEvtStr = J.encode outEvt{J.bodyOutputEvent = J.OutputEventBody "console" msg Nothing }
  sendEvent outEvtStr

-- |
--
--
sendStdoutEvent :: MVar DebugContextData -> String -> IO ()
sendStdoutEvent mvarCtx msg = do
  resSeq <- getIncreasedResponseSequence mvarCtx
  let outEvt    = J.defaultOutputEvent resSeq
      outEvtStr = J.encode outEvt{J.bodyOutputEvent = J.OutputEventBody "stdout" msg Nothing }
  sendEvent outEvtStr

-}
-- |
--
--
sendErrorEvent :: MVar DebugContextData -> String -> IO ()
sendErrorEvent mvarCtx msg = do
  resSeq <- getIncreasedResponseSequence mvarCtx
  let outEvt    = J.defaultOutputEvent resSeq
      outEvtStr = J.encode outEvt{J.bodyOutputEvent = J.OutputEventBody "stderr" msg Nothing }
  sendEvent outEvtStr

-- |=====================================================================
--
-- Handlers

-- |
--
initializeRequestHandler :: MVar DebugContextData -> J.InitializeRequest -> IO ()
initializeRequestHandler mvarCtx req@(J.InitializeRequest seq _) = flip E.catches handlers $ do
  resSeq <- getIncreasedResponseSequence mvarCtx
  let capa = J.InitializeResponseCapabilites True True True True False False False False False True
      res  = J.InitializeResponse resSeq "response" seq True "initialize" "" capa

  sendResponse2 mvarCtx $ J.encode res

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["initialize request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse $ J.encode $ J.errorInitializeResponse resSeq req msg
      sendErrorEvent mvarCtx msg

{-
-- |
--
launchRequestHandler :: MVar DebugContextData -> J.LaunchRequest -> IO ()
launchRequestHandler mvarCtx req@(J.LaunchRequest _ _ _ args) = flip E.catches handlers $ do
  let ws = J.workspaceLaunchRequestArguments args
      su = J.startupLaunchRequestArguments args
      logFile     = J.logFileLaunchRequestArguments args
      logLevelStr = J.logLevelLaunchRequestArguments args
      prmptStr    = J.ghciPromptLaunchRequestArguments args
      cmdStr      = J.ghciCmdLaunchRequestArguments args

  -- コンテキストデータの保持
  ctx <- takeMVar mvarCtx
  putMVar mvarCtx ctx {
      workspaceDebugContextData = ws
    , startupDebugContextData   = su
    }

  -- ロギング設定
  logLevel <- case readMay logLevelStr of
    Just lv -> return lv
    Nothing -> do
      sendErrorEvent mvarCtx $ "log priority is invalid. WARNING set. [" ++ logLevelStr ++ "]\n"
      return WARNING

  setupLogger logFile logLevel

  logRequest $ show req

  -- tasks.jsonファイルの準備
  prepareTasksJsonFile mvarCtx ws


  -- ghciのランチ
  runGHCi mvarCtx cmdStr ws prmptStr >>= ghciLaunched


  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["launch request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse $ J.encode $ J.errorLaunchResponse resSeq req msg
      sendErrorEvent mvarCtx $ msg ++ "\n"

    -- |
    --
    prepareTasksJsonFile :: MVar DebugContextData -> FilePath -> IO ()
    prepareTasksJsonFile mvarCtx ws = do
      let jsonFile = ws </> ".vscode" </> "tasks.json"

      doesFileExist jsonFile >>= \case
        True  -> infoM _LOG_NAME $ "tasks.json file exists. " ++ jsonFile
        False -> do
          sendConsoleEvent mvarCtx $ "create tasks.json file. " ++ jsonFile ++ "\n"
          saveFileLBS jsonFile _TASKS_JSON_FILE_CONTENTS

    -- |
    --
    ghciLaunched (Left err) = do
      let msg = L.intercalate " " ["ghci launch error.", err]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponse $ J.encode $ J.errorLaunchResponse resSeq req msg
      sendErrorEvent mvarCtx $ msg ++ "\n"

    ghciLaunched (Right ghciProc) = do
      ctx <- takeMVar mvarCtx
      putMVar mvarCtx ctx{ghciProcessDebugContextData = Just ghciProc}

      startupRes <- loadHsFile mvarCtx (J.startupLaunchRequestArguments args)

      when (False == startupRes) $ do
        let msg = L.intercalate " " ["startup load error.", J.startupLaunchRequestArguments args]
        sendErrorEvent mvarCtx $ msg ++ "\n"

      -- レスポンスとinitializedイベント送信
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponseL $ J.encode $ J.defaultLaunchResponse resSeq req

      resSeq <- getIncreasedResponseSequence mvarCtx
      sendEventL $ J.encode $ J.defaultInitializedEvent resSeq

      resSeq <- getIncreasedResponseSequence mvarCtx
      let stopEvt    = J.defaultStoppedEvent resSeq
          stopEvtStr = J.encode stopEvt
      sendEventL stopEvtStr

      -- ファイル変更ウォッチの開始
      watch mvarCtx

      --sendStdoutEvent mvarCtx $ J.ghciPromptLaunchRequestArguments args

-- |
--
configurationDoneRequestHandler :: MVar DebugContextData -> J.ConfigurationDoneRequest -> IO ()
configurationDoneRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req
  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["configurationDone request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponseL $ J.encode $ J.errorConfigurationDoneResponse resSeq req msg
      sendErrorEvent mvarCtx msg

    infoMsg = [
              ""
            , "  Now, ghci launched and configuration done."
            , "  Press F5 to start debugging."
            , "  Or modify source code. it will be loaded to ghci automatically."
            , " "
            ]

    withProcess Nothing = do
      errorM _LOG_NAME "[disconnectRequestHandler] ghci not started."
      sendErrorEvent mvarCtx "[disconnectRequestHandler] ghci not started."

    withProcess (Just ghciProc) = do
      sendConsoleEvent mvarCtx $ L.intercalate "\n" infoMsg
      sendStdoutEvent mvarCtx $ G.promptGHCiProcess ghciProc

      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponseL $ J.encode $ J.defaultConfigurationDoneResponse resSeq req


-- |
--
disconnectRequestHandler :: MVar DebugContextData -> J.DisconnectRequest -> IO ()
disconnectRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req
  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["disconnect request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponseL $ J.encode $ J.errorDisconnectResponse resSeq req msg
      sendErrorEvent mvarCtx msg

    withProcess Nothing = do
      errorM _LOG_NAME "[disconnectRequestHandler] ghci not started."
      sendErrorEvent mvarCtx "[disconnectRequestHandler] ghci not started."

    withProcess (Just ghciProc) = G.quit ghciProc outHdl >>= withExitCode

    withExitCode (Left err)  = do
      errorM _LOG_NAME $ "[disconnectRequestHandler] ghci quit error. " ++ err
      sendErrorEvent mvarCtx $ "[disconnectRequestHandler] ghci quit error. " ++ err

    withExitCode (Right code) = do
      infoM _LOG_NAME $ show code
      sendStdoutEvent mvarCtx $ show code
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponseL $ J.encode $ J.defaultDisconnectResponse resSeq req

    outHdl msg = do
      sendStdoutEvent mvarCtx msg
      infoM _LOG_NAME msg


-- |
--
setBreakpointsRequestHandler :: MVar DebugContextData -> J.SetBreakpointsRequest -> IO ()
setBreakpointsRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  ctx <- readMVar mvarCtx
  let cwd     = workspaceDebugContextData ctx
      args    = J.argumentsSetBreakpointsRequest req
      source  = J.sourceSetBreakpointsRequestArguments args
      path    = J.pathSource source
      reqBps  = J.breakpointsSetBreakpointsRequestArguments args
      bps     = map (convBp cwd path) reqBps

  delete path
  resBody <- insert bps

  resSeq <- getIncreasedResponseSequence mvarCtx
  let res    = J.defaultSetBreakpointsResponse resSeq req
      resStr = J.encode res{J.bodySetBreakpointsResponse = J.SetBreakpointsResponseBody resBody}
  sendResponseL resStr

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["setBreakpoints request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponseL $ J.encode $ J.errorSetBreakpointsResponse resSeq req msg
      sendErrorEvent mvarCtx msg

    convBp cwd path (J.SourceBreakpoint lineNo _ cond) =
      BreakPointData {
        nameBreakPointData       = src2mod cwd path
      , filePathBreakPointData   = path
      , lineNoBreakPointData     = lineNo
      , breakNoBreakPointData    = Nothing
      , conditionBreakPointData  = normalizeCond cond
      }

    normalizeCond Nothing = Nothing
    normalizeCond (Just c)
      | null (U.strip c) = Nothing
      | otherwise = Just c

    delete path = do
      ctx <- takeMVar mvarCtx
      let bps = breakPointDatasDebugContextData ctx
          newBps = MAP.filterWithKey (\(p,_) _-> path /= p) bps
          delBps = MAP.elems $ MAP.filterWithKey (\(p,_) _-> path == p) bps

      putMVar mvarCtx ctx{breakPointDatasDebugContextData = newBps}

      debugM _LOG_NAME $ "del bps:" ++ show delBps

      mapM_ (deleteBreakPointOnGHCi mvarCtx) delBps


    insert reqBps = do
      results <- mapM insertInternal reqBps
      let addBps  = filter (\(_, (J.Breakpoint _ verified _ _ _ _)) -> verified) results
          resData = map snd results

      debugM _LOG_NAME $ "add bps:" ++ show addBps
      debugM _LOG_NAME $ "response bps:" ++ show resData

      ctx <- takeMVar mvarCtx
      let bps    = breakPointDatasDebugContextData ctx
          newBps = foldr (\v@(BreakPointData _ p l _ _)->MAP.insert (p,l) v) bps $ map fst results
      putMVar mvarCtx ctx{breakPointDatasDebugContextData = newBps}

      return resData


    insertInternal reqBp@(BreakPointData modName filePath lineNo _ _) = do
      let src = J.Source (Just modName) filePath Nothing Nothing

      addBreakPointOnGHCi mvarCtx reqBp >>= \case
        Right no -> do
          --putStrLnStdout mvarCtx $ "set breakpoint on " ++ filePathBreakPointData reqBp ++ ":L" ++ show (lineNoBreakPointData reqBp)
          return (reqBp{breakNoBreakPointData = Just no}, J.Breakpoint (Just no) True "" src lineNo 1)
        Left err -> return (reqBp, J.Breakpoint Nothing False err src lineNo 1)

-- |
--
setFunctionBreakpointsRequestHandler :: MVar DebugContextData -> J.SetFunctionBreakpointsRequest -> IO ()
setFunctionBreakpointsRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  let args    = J.argumentsSetFunctionBreakpointsRequest req
      reqBps  = J.breakpointsSetFunctionBreakpointsRequestArguments args
      bps     = map convBp reqBps

  delete
  resBody <- insert bps

  resSeq <- getIncreasedResponseSequence mvarCtx
  let res    = J.defaultSetFunctionBreakpointsResponse resSeq req
      resStr = J.encode res{J.bodySetFunctionBreakpointsResponse = J.SetFunctionBreakpointsResponseBody resBody}
  sendResponseL resStr

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["setBreakpoints request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponseL $ J.encode $ J.errorSetFunctionBreakpointsResponse resSeq req msg
      sendErrorEvent mvarCtx msg

    convBp (J.FunctionBreakpoint name cond) =
      BreakPointData {
        nameBreakPointData       = name
      , filePathBreakPointData   = ""
      , lineNoBreakPointData     = -1
      , breakNoBreakPointData    = Nothing
      , conditionBreakPointData  = normalizeCond cond
      }

    normalizeCond Nothing = Nothing
    normalizeCond (Just c)
      | null (U.strip c) = Nothing
      | otherwise = Just c

    delete = do
      ctx <- takeMVar mvarCtx
      let bps = functionBreakPointDatasDebugContextData ctx
          delBps = MAP.elems bps

      putMVar mvarCtx ctx{functionBreakPointDatasDebugContextData = MAP.fromList []}

      debugM _LOG_NAME $ "del bps:" ++ show delBps

      mapM_ (deleteBreakPointOnGHCi mvarCtx) delBps


    insert reqBps = do
      results <- mapM insertInternal reqBps
      let addBps  = filter (\(_, (J.Breakpoint _ verified _ _ _ _)) -> verified) results
          resData = map snd results

      debugM _LOG_NAME $ "add funBPs:" ++ show addBps
      debugM _LOG_NAME $ "response funBPs:" ++ show resData

      ctx <- takeMVar mvarCtx
      let bps    = functionBreakPointDatasDebugContextData ctx
          newBps = foldr (\v@(BreakPointData _ p l _ _)->MAP.insert (p,l) v) bps $ map fst results
      putMVar mvarCtx ctx{functionBreakPointDatasDebugContextData = newBps}

      return resData


    insertInternal reqBp@(BreakPointData funcName _ _ _ _) = do
      addFunctionBreakPointOnGHCi mvarCtx reqBp >>= \case
        Right (no, (G.SourcePosition path sl sc _ _)) -> do
          --putStrLnStdout mvarCtx $ "set breakpoint on " ++ filePathBreakPointData reqBp ++ ":L" ++ show (lineNoBreakPointData reqBp)
          return ( reqBp{ breakNoBreakPointData  = Just no
                        , filePathBreakPointData = path
                        , lineNoBreakPointData   = sl
                        }
                 , J.Breakpoint (Just no) True "" (J.Source (Just funcName) path Nothing Nothing) sl sc)
        Left err -> return (reqBp, J.Breakpoint Nothing False err (J.Source (Just funcName) "" Nothing Nothing) (-1) (-1))

-- |
--
--
continueRequestHandler :: MVar DebugContextData -> J.ContinueRequest -> IO ()
continueRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req
  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["continue request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponseL $ J.encode $ J.errorContinueResponse resSeq req msg
      sendErrorEvent mvarCtx msg

    withProcess Nothing = do
      errorM _LOG_NAME "[continueRequestHandler] ghci not started."
      sendErrorEvent mvarCtx "[continueRequestHandler] ghci not started."

    withProcess (Just _) = do
      resSeq <- getIncreasedResponseSequence mvarCtx
      let resStr = J.encode $ J.defaultContinueResponse resSeq req
      sendResponseL resStr

      startDebug mvarCtx


-- |
--
--
nextRequestHandler :: MVar DebugContextData -> J.NextRequest -> IO ()
nextRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  ctx <- readMVar mvarCtx
  case debugStoppedPosDebugContextData ctx of
    Nothing -> do
      resSeq <- getIncreasedResponseSequence mvarCtx
      let res    = J.defaultNextResponse resSeq req
          resStr = J.encode res{J.successNextResponse = False, J.messageNextResponse = "debug is initialized but not started yet. press F5(continue)."}
      sendResponseL resStr
    Just _ -> next

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["stepOver request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponseL $ J.encode $ J.errorNextResponse resSeq req msg
      sendErrorEvent mvarCtx msg

    next = ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess

    withProcess Nothing = do
      errorM _LOG_NAME "[nextRequestHandler] ghci not started."
      sendErrorEvent mvarCtx "[nextRequestHandler] ghci not started."

    withProcess (Just ghciProc) = G.stepLocal ghciProc outHdl >>= \case
      Left err  -> do
        infoM _LOG_NAME $ show err
        --putStrLnStdout mvarCtx $ show err

        resSeq <- getIncreasedResponseSequence mvarCtx
        let terminatedEvt    = J.defaultTerminatedEvent resSeq
            terminatedEvtStr = J.encode terminatedEvt
        sendEventL terminatedEvtStr

      Right pos -> do
        ctx <- takeMVar mvarCtx
        putMVar mvarCtx ctx{debugStoppedPosDebugContextData = Just pos}

        resSeq <- getIncreasedResponseSequence mvarCtx
        let res    = J.defaultNextResponse resSeq req
            resStr = J.encode res
        sendResponseL resStr

        resSeq <- getIncreasedResponseSequence mvarCtx
        let stopEvt    = J.defaultStoppedEvent resSeq
            stopEvtStr = J.encode stopEvt
        sendEventL stopEvtStr

    outHdl msg = do
      sendStdoutEvent mvarCtx msg
      infoM _LOG_NAME msg


-- |
--
--
stepInRequestHandler :: MVar DebugContextData -> J.StepInRequest -> IO ()
stepInRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  ctx <- readMVar mvarCtx
  case debugStoppedPosDebugContextData ctx of
    Nothing -> do
      resSeq <- getIncreasedResponseSequence mvarCtx
      let res    = J.defaultStepInResponse resSeq req
          resStr = J.encode res{J.successStepInResponse = False, J.messageStepInResponse = "debug is initialized but not started yet. press F5(continue)."}
      sendResponseL resStr
    Just _ -> stepIn

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["stepIn request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponseL $ J.encode $ J.errorStepInResponse resSeq req msg
      sendErrorEvent mvarCtx msg

    stepIn = ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess

    withProcess Nothing = do
      errorM _LOG_NAME "[stepInRequestHandler] ghci not started."
      sendErrorEvent mvarCtx "[stepInRequestHandler] ghci not started."

    withProcess (Just ghciProc) = G.step ghciProc outHdl >>= \case
      Left err  -> do
        infoM _LOG_NAME $ show err
        --putStrLnStdout mvarCtx $ show err

        resSeq <- getIncreasedResponseSequence mvarCtx
        let terminatedEvt    = J.defaultTerminatedEvent resSeq
            terminatedEvtStr = J.encode terminatedEvt
        sendEventL terminatedEvtStr

      Right pos -> do
        ctx <- takeMVar mvarCtx
        putMVar mvarCtx ctx{debugStoppedPosDebugContextData = Just pos}

        resSeq <- getIncreasedResponseSequence mvarCtx
        let res    = J.defaultStepInResponse resSeq req
            resStr = J.encode res
        sendResponseL resStr

        resSeq <- getIncreasedResponseSequence mvarCtx
        let stopEvt    = J.defaultStoppedEvent resSeq
            stopEvtStr = J.encode stopEvt
        sendEventL stopEvtStr

    outHdl msg = do
      sendStdoutEvent mvarCtx msg
      infoM _LOG_NAME msg


-- |
--
--
stackTraceRequestHandler :: MVar DebugContextData -> J.StackTraceRequest -> IO ()
stackTraceRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  ctx <- readMVar mvarCtx
  case debugStoppedPosDebugContextData ctx of
    Nothing -> do
      resSeq <- getIncreasedResponseSequence mvarCtx
      let body = J.StackTraceBody [] 0
          res  = J.defaultStackTraceResponse resSeq req
          resStr = J.encode $ res{J.bodyStackTraceResponse = body}
      sendResponseL resStr

    Just rangeData -> do
      frames <- createStackFrames rangeData
      debugM _LOG_NAME $ show frames

      resSeq <- getIncreasedResponseSequence mvarCtx
      let body   = J.StackTraceBody (reverse frames) (length frames)
          res    = J.defaultStackTraceResponse resSeq req
          resStr = J.encode $ res{J.bodyStackTraceResponse = body}
      sendResponseL resStr

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["stackTrace request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponseL $ J.encode $ J.errorStackTraceResponse resSeq req msg
      sendErrorEvent mvarCtx msg

    createStackFrames pos = ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess pos

    withProcess pos Nothing = do
      errorM _LOG_NAME "[stackTraceRequestHandler] ghci not started."
      sendErrorEvent mvarCtx "[stackTraceRequestHandler] ghci not started."
      defaultFrame pos

    withProcess pos (Just ghciProc) = G.history ghciProc outHdl >>= \case
      Left err   -> do
        errorM _LOG_NAME $ show err
        sendErrorEvent mvarCtx $ show err
        defaultFrame pos

      Right dats -> do
        cwd <- workspaceDebugContextData <$> readMVar mvarCtx
        cfs <- defaultFrame pos
        foldM (convTrace2Frame cwd) cfs dats

    convTrace2Frame cwd xs (G.StackFrame traceId funcName (G.SourcePosition file sl sc el ec)) = return $
      J.StackFrame traceId funcName (J.Source (Just (src2mod cwd file)) file Nothing Nothing) sl sc el ec : xs

    defaultFrame (G.SourcePosition file sl sc el ec) = do
      ctx <- readMVar mvarCtx
      let cwd = workspaceDebugContextData ctx
          csf = J.StackFrame 0 "[BP]" (J.Source (Just (src2mod cwd file)) file Nothing Nothing) sl sc el ec
      return  [csf]

    outHdl msg = do
      --sendStdoutEvent mvarCtx msg
      infoM _LOG_NAME msg


-- |
--
--
scopesRequestHandler :: MVar DebugContextData -> J.ScopesRequest -> IO ()
scopesRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  let args    = J.argumentsScopesRequest req
      traceId = J.frameIdScopesArguments args

  moveFrame mvarCtx traceId

  resSeq <- getIncreasedResponseSequence mvarCtx
  let resStr = J.encode $ J.defaultScopesResponse resSeq req
  sendResponseL resStr

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["scopes request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponseL $ J.encode $ J.errorScopesResponse resSeq req msg
      sendErrorEvent mvarCtx msg

-- |
--
--
variablesRequestHandler :: MVar DebugContextData -> J.VariablesRequest -> IO ()
variablesRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  vals <- currentBindings

  resSeq <- getIncreasedResponseSequence mvarCtx
  let res = J.defaultVariablesResponse resSeq req
      resStr = J.encode $ res{J.bodyVariablesResponse = J.VariablesBody vals}
  sendResponseL resStr

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["variables request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponseL $ J.encode $ J.errorVariablesResponse resSeq req msg
      sendErrorEvent mvarCtx msg

    currentBindings = ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess

    withProcess Nothing = do
      errorM _LOG_NAME "[variablesRequestHandler] ghci not started."
      sendErrorEvent mvarCtx "[variablesRequestHandler] ghci not started."
      return []

    withProcess (Just ghciProc) = G.bindings ghciProc outHdl >>= \case
      Left err   -> do
        errorM _LOG_NAME $ show err
        sendErrorEvent mvarCtx $ show err
        return []

      Right dats -> return $ map convBind2Vals dats

    convBind2Vals (G.BindingData varName modName val) = J.Variable varName modName val 0

    outHdl msg = do
      --sendStdoutEvent mvarCtx msg
      infoM _LOG_NAME msg


-- |
--
--
threadsRequestHandler :: MVar DebugContextData -> J.ThreadsRequest -> IO ()
threadsRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  resSeq <- getIncreasedResponseSequence mvarCtx
  let resStr = J.encode $ J.defaultThreadsResponse resSeq req
  sendResponseL resStr

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["threads request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponseL $ J.encode $ J.errorThreadsResponse resSeq req msg
      sendErrorEvent mvarCtx msg


-- |
--
--
evaluateRequestHandler :: MVar DebugContextData -> J.EvaluateRequest -> IO ()
evaluateRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  let (J.EvaluateArguments exp _ ctx) = J.argumentsEvaluateRequest req
  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess ctx exp

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["evaluate request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponseL $ J.encode $ J.errorEvaluateResponse resSeq req msg
      sendErrorEvent mvarCtx msg

    withProcess _ _ Nothing = do
      errorM _LOG_NAME "[evaluateRequestHandler] ghci not started."
      sendErrorEvent mvarCtx "[evaluateRequestHandler] ghci not started."

    withProcess "watch" exp (Just ghciProc) = G.showType ghciProc outHdl exp >>= \case
      Left err -> do
        errorM _LOG_NAME $ show err
        evaluateResponse err ""

      Right typeStr -> case isFunction typeStr of
        True  -> evaluateResponse "" (getOnlyType typeStr)
        False -> G.force ghciProc outHdl exp >>= \case
          Right valStr -> evaluateResponse (getOnlyValue valStr) (getOnlyType typeStr)
          Left _ -> evaluateResponse "" (getOnlyType typeStr)

    withProcess "hover" exp (Just ghciProc) = G.showType ghciProc outHdl exp >>= \case
      Left err -> do
        errorM _LOG_NAME $ show err
        evaluateResponse err ""
      Right typeStr -> evaluateResponse typeStr (getOnlyType typeStr)

    withProcess _ exp (Just ghciProc) = G.exec ghciProc outHdl exp >>= \case
      Left err   -> do
        errorM _LOG_NAME $ show err
        sendErrorEvent mvarCtx $ show err

      Right cmdStr -> do
        -- promptを消す
        let result = U.join " " . map U.strip . init . lines $ cmdStr
        evaluateResponse result ""
        sendStdoutEvent mvarCtx $ "\n" ++ G.promptGHCiProcess ghciProc

    outHdl msg = do
      --sendStdoutEvent mvarCtx msg
      infoM _LOG_NAME msg

    isFunction str = case parse isFunctionParser "isFunction" str of
      Right _ -> True
      Left _  -> False

    isFunctionParser = manyTill anyChar (string "->")

    evaluateResponse msg typeStr = do
      resSeq <- getIncreasedResponseSequence mvarCtx
      let body   = J.EvaluateBody msg typeStr 0
          res    = J.defaultEvaluateResponse resSeq req
          resStr = J.encode res{J.bodyEvaluateResponse = body}
      sendResponseL resStr

    -- |
    --  force結果のパーサ
    --
    --  parser of
    --    Phoityne>>= :force x
    --    x = 8
    --    Phoityne>>=
    --
    getOnlyValue :: String -> String
    getOnlyValue str = case parse getOnlyValueParser "getOnlyValue" str of
      Right vals -> vals
      Left _ -> str
      where
        getOnlyValueParser = do
          _ <- manyTill anyChar (string " = ")
          manyTill anyChar eof

    -- |
    --  type結果のパーサ
    --
    --  parser of
    --    Phoityne>>= :type x
    --    x :: Int -> Int
    --    Phoityne>>=
    --
    getOnlyType :: String -> String
    getOnlyType str = case parse getOnlyTypeParser "getOnlyType" str of
      Right vals -> vals
      Left _ -> str
      where
        getOnlyTypeParser = do
          _ <- manyTill anyChar (string " :: ")
          manyTill anyChar eof


-- |
--
--
completionsRequestHandler :: MVar DebugContextData -> J.CompletionsRequest -> IO ()
completionsRequestHandler mvarCtx req = flip E.catches handlers $ do
  logRequest $ show req

  let (J.CompletionsArguments _ key _ _) = J.argumentsCompletionsRequest req
  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess key

  where
    handlers = [ E.Handler someExcept ]
    someExcept (e :: E.SomeException) = do
      let msg = L.intercalate " " ["completions request error.", show req, show e]
      resSeq <- getIncreasedResponseSequence mvarCtx
      sendResponseL $ J.encode $ J.errorCompletionsResponse resSeq req msg
      sendErrorEvent mvarCtx msg

    withProcess _ Nothing = do
      errorM _LOG_NAME "[completionsRequestHandler] ghci not started."
      sendErrorEvent mvarCtx "[completionsRequestHandler] ghci not started."

    withProcess key (Just ghciProc) = G.complete ghciProc outHdl key 50 >>= \case
      Left err -> do
        errorM _LOG_NAME $ show err
        resSeq <- getIncreasedResponseSequence mvarCtx
        let resStr = J.encode $ J.errorCompletionsResponse resSeq req $ show err
        sendResponseL resStr

      Right xs -> do
        resSeq <- getIncreasedResponseSequence mvarCtx
        let bd = J.CompletionsResponseBody $ map createItem xs
            res = J.defaultCompletionsResponse resSeq req

        let resStr = J.encode $ res {J.bodyCompletionsResponse = bd}
        sendResponseL resStr

    createItem (':':xs) = J.CompletionsItem xs
    createItem xs = J.CompletionsItem xs

    outHdl msg = do
      --sendStdoutEvent mvarCtx msg
      infoM _LOG_NAME msg


-- |=====================================================================
--
--  utility


-- |
--
--
logRequest :: String -> IO ()
logRequest reqStr = do
  let msg = L.intercalate " " ["[REQUEST]", reqStr]
  infoM _LOG_NAME msg



-- |
--
--
src2mod :: FilePath -> FilePath -> String
src2mod cwd src
  | length cwd >= length src = ""
  | otherwise = L.intercalate "."
      $ map takeBaseName
      $ reverse
      $ takeWhile startUpperCase
      $ reverse
      $ splitOneOf [_SEP_WIN, _SEP_UNIX]
      $ drop (length cwd) src

  where
    startUpperCase modName
      | null modName = True
      | otherwise = isUpper $ head modName
-}

-- |
--
--
getIncreasedResponseSequence :: MVar DebugContextData -> IO Int
getIncreasedResponseSequence mvarCtx = do
  ctx <- takeMVar mvarCtx
  let resSec = 1 + resSeqDebugContextData ctx
  putMVar mvarCtx ctx{resSeqDebugContextData = resSec}
  return resSec

{-
-- |
--
--
runGHCi :: MVar DebugContextData -> String -> FilePath -> String -> IO (Either G.ErrorData G.GHCiProcess)
runGHCi mvarCtx cmdStr cwd pmt = do
  let cmdList = filter (not.null) $ U.split " " cmdStr
      cmd  = head cmdList
      opts = tail cmdList

  G.start outHdl cmd opts cwd pmt

  where
    outHdl msg = do
      sendStdoutEvent mvarCtx msg
      infoM _LOG_NAME msg


-- |
--
--
loadHsFile :: MVar DebugContextData -> FilePath -> IO Bool
loadHsFile mvarCtx path = ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= \case
  Nothing -> do
    errorM _LOG_NAME $ "load file fail.[" ++ path ++ "]" ++ " ghci not started."
    sendErrorEvent mvarCtx $ "load file fail.[" ++ path ++ "]" ++ " ghci not started."
    return False
  Just ghciProc -> G.loadFile ghciProc outHdl path >>= withFileLoadResult ghciProc

  where
    outHdl msg = do
      sendStdoutEvent mvarCtx msg
      infoM _LOG_NAME msg

    withFileLoadResult _ (Left err) = do
      errorM _LOG_NAME $ "load file fail.[" ++ path ++ "]" ++ " " ++ err
      sendErrorEvent mvarCtx $ "load file fail.[" ++ path ++ "]" ++ " " ++ err
      return False

    withFileLoadResult ghciProc (Right mods) = G.loadModule ghciProc outHdl mods >>= \case
      Left err -> do
        errorM _LOG_NAME $ "load module fail. " ++ show mods ++ " " ++ err
        sendErrorEvent mvarCtx $ "load module fail. " ++ show mods ++ " " ++ err
        return False
      Right _ -> return True


-- |
--  ブレークポイントをGHCi上でdeleteする
--
deleteBreakPointOnGHCi :: MVar DebugContextData -> BreakPointData -> IO ()
deleteBreakPointOnGHCi mvarCtx bp@(BreakPointData _ _ _ (Just breakNo) _) =
  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= \case
    Nothing -> do
      errorM _LOG_NAME $ "[deleteBreakPointOnGHCi] ghci not started. " ++ show bp
      sendErrorEvent mvarCtx $ "[deleteBreakPointOnGHCi] ghci not started. " ++ show bp
    Just ghciProc -> G.delete ghciProc outHdl breakNo >>= withResult

  where
    outHdl msg = do
      sendStdoutEvent mvarCtx msg
      infoM _LOG_NAME msg

    withResult (Left err) = do
      errorM _LOG_NAME $ "[deleteBreakPointOnGHCi] " ++ err ++ " " ++ show bp
      sendErrorEvent mvarCtx $ "[deleteBreakPointOnGHCi] " ++ err ++ " " ++ show bp

    withResult (Right _) = return ()

deleteBreakPointOnGHCi mvarCtx bp = do
  let err = "[deleteBreakPointOnGHCi] invalid delete break point. "  ++ show bp
  sendErrorEvent mvarCtx err
  errorM _LOG_NAME err

-- |
--  GHCi上でブレークポイントを追加する
--
addBreakPointOnGHCi :: MVar DebugContextData -> BreakPointData -> IO (Either String Int)
addBreakPointOnGHCi mvarCtx bp@(BreakPointData modName _ lineNo _ _) =
  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= \case
    Nothing -> do
      errorM _LOG_NAME $ "[addBreakPointOnGHCi] ghci not started. " ++ show bp
      return $ Left $ "[addBreakPointOnGHCi] ghci not started. " ++ show bp
    Just ghciProc -> G.setBreak ghciProc outHdl modName lineNo

  where
    outHdl msg = do
      sendStdoutEvent mvarCtx msg
      infoM _LOG_NAME msg

-- |
--  GHCi上で関数ブレークポイントを追加する
--
addFunctionBreakPointOnGHCi :: MVar DebugContextData -> BreakPointData -> IO (Either String (Int, G.SourcePosition))
addFunctionBreakPointOnGHCi mvarCtx bp@(BreakPointData name _ _ _ _) =
  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= \case
    Nothing -> do
      errorM _LOG_NAME $ "[addFunctionBreakPointOnGHCi] ghci not started. " ++ show bp
      return $ Left $ "[addFunctionBreakPointOnGHCi] ghci not started. " ++ show bp
    Just ghciProc -> G.setFuncBreak ghciProc outHdl name

  where
    outHdl msg = do
      sendStdoutEvent mvarCtx msg
      infoM _LOG_NAME msg

-- |
--  Loggerのセットアップ
--
setupLogger :: FilePath -> Priority -> IO ()
setupLogger logFile level = do
--  level <- case readMay logLevel of
--    Just a  -> return a
--    Nothing -> E.throwIO . E.userError $ "invalid log level[" ++ logLevel ++ "]"

  logStream <- openFile logFile AppendMode
  hSetEncoding logStream utf8

  logH <- LHS.streamHandler logStream level

  let logHandle  = logH {LHS.closeFunc = hClose}
      logFormat  = L.tfLogFormatter _LOG_FORMAT_DATE _LOG_FORMAT
      logHandler = LH.setFormatter logHandle logFormat

  L.updateGlobalLogger L.rootLoggerName $ L.setHandlers ([] :: [LHS.GenericHandler Handle])
  L.updateGlobalLogger _LOG_NAME $ L.setHandlers [logHandler]
  L.updateGlobalLogger _LOG_NAME $ L.setLevel level

-- |
--
--
watch :: MVar DebugContextData -> IO ()
watch mvarCtx = do
  _ <- forkIO $ watchFiles mvarCtx
  return ()

watchFiles :: MVar DebugContextData -> IO ()
watchFiles mvarCtx = do
  FSN.withManagerConf FSN.defaultConfig{FSN.confDebounce  = FSN.Debounce 1} $ \mgr -> do

    ctx <- readMVar mvarCtx
    let dir = workspaceDebugContextData ctx

    infoM _LOG_NAME $ "start watch files in [" ++ dir ++ "]"
    _ <- FSN.watchTree mgr dir hsFilter action

    forever $ threadDelay 1000000

  return ()

  where
    hsFilter event = U.endswith _HS_FILE_EXT $ FSN.eventPath event

    action event = do

      ctx <- readMVar mvarCtx
      withDebugStarted event $ debugStartedDebugContextData ctx

    withDebugStarted _ True = do
      resSeq <- getIncreasedResponseSequence mvarCtx
      let terminatedEvt    = J.defaultTerminatedEvent resSeq
          terminatedEvtStr = J.encode terminatedEvt{J.bodyTerminatedEvent = J.TerminatedEventBody True}
      sendEventL terminatedEvtStr

    withDebugStarted event False = do
      ctx <- takeMVar mvarCtx
      putMVar mvarCtx ctx{modifiedDebugContextData = True}
      loadHsFile mvarCtx (FSN.eventPath event) >> return ()


-- |
--
--
moveFrame :: MVar DebugContextData -> Int -> IO ()
moveFrame mvarCtx traceId = ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess
  where
    withProcess Nothing = do
      errorM _LOG_NAME "[moveFrame] ghci not started."
      sendErrorEvent mvarCtx "[moveFrame] ghci not started."

    withProcess (Just ghciProc) = do
      ctx <- readMVar mvarCtx
      let curTraceId = currentFrameIdDebugContextData ctx
          moveCount  = curTraceId - traceId
          traceCmd   = if 0 > moveCount then G.back ghciProc outHdl
                        else G.forward ghciProc outHdl

      -- _ <- traceCmd (abs moveCount)
      mapM_ traceCmd [1..(abs moveCount)]

      ctx <- takeMVar mvarCtx
      putMVar mvarCtx ctx{currentFrameIdDebugContextData = traceId}

    outHdl msg = do
      sendStdoutEvent mvarCtx msg
      infoM _LOG_NAME msg

-- |
--
--
startDebug :: MVar DebugContextData -> IO ()
startDebug mvarCtx = do
  ctx <- readMVar mvarCtx
  let started = debugStartedDebugContextData ctx

  ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= startDebugInternal started


  where
    startDebugInternal _ Nothing = do
      errorM _LOG_NAME "[startDebug] ghci not started."
      sendErrorEvent mvarCtx "[startDebug] ghci not started."

    startDebugInternal True (Just ghciProc) = G.trace ghciProc outHdl >>= \case
      Left err  -> do
        infoM _LOG_NAME $ show err
        --putStrLnStdout mvarCtx $ show err

        resSeq <- getIncreasedResponseSequence mvarCtx
        let terminatedEvt    = J.defaultTerminatedEvent resSeq
            terminatedEvtStr = J.encode terminatedEvt
        sendEventL terminatedEvtStr

      Right pos -> continueWithSourcePosition mvarCtx pos

    startDebugInternal False (Just ghciProc) = do
      ctx <- readMVar mvarCtx
      withModified ghciProc $ modifiedDebugContextData ctx

    withModified _ True = do
      resSeq <- getIncreasedResponseSequence mvarCtx
      let terminatedEvt    = J.defaultTerminatedEvent resSeq
          terminatedEvtStr = J.encode terminatedEvt{J.bodyTerminatedEvent = J.TerminatedEventBody True}
      sendEventL terminatedEvtStr

    withModified ghciProc False = G.traceMain ghciProc outHdl >>= \case
      Left err  -> do
        infoM _LOG_NAME $ show err
        --putStrLnStdout mvarCtx $ show err

        resSeq <- getIncreasedResponseSequence mvarCtx
        let terminatedEvt    = J.defaultTerminatedEvent resSeq
            terminatedEvtStr = J.encode terminatedEvt
        sendEventL terminatedEvtStr

      Right pos -> do
        ctx <- takeMVar mvarCtx
        putMVar mvarCtx ctx{currentFrameIdDebugContextData = 0, debugStartedDebugContextData = True}
        continueWithSourcePosition mvarCtx pos

    outHdl msg = do
      sendStdoutEvent mvarCtx msg
      infoM _LOG_NAME msg


-- |
--
--
continueWithSourcePosition :: MVar DebugContextData -> G.SourcePosition -> IO ()
continueWithSourcePosition mvarCtx pos = do
  ctx <- readMVar mvarCtx

  let bpKey = getKeyOfSourcePosition pos
      bpMap = breakPointDatasDebugContextData ctx
      funcBpMap = functionBreakPointDatasDebugContextData ctx

  case MAP.lookup bpKey bpMap of
    Nothing -> case MAP.lookup bpKey funcBpMap of
      Nothing -> do
        errorM _LOG_NAME $ "breakpoint not found." ++ show bpKey
        sendStopEvent
      Just condCmd -> continueWithCondCmd $ conditionBreakPointData condCmd
    Just condCmd -> continueWithCondCmd $ conditionBreakPointData condCmd

  where

    -- |
    --
    continueWithCondCmd Nothing = do
      infoM _LOG_NAME "no condition breakpoint"
      sendStopEvent
    continueWithCondCmd (Just condStr) = ghciProcessDebugContextData <$> (readMVar mvarCtx) >>= withProcess condStr

    withProcess _ Nothing = do
      errorM _LOG_NAME "[continueWithSourcePosition] ghci not started."
      sendErrorEvent mvarCtx "[continueWithSourcePosition] ghci not started."
    withProcess condStr (Just ghciProc) = do
      forceBindings ghciProc outHdl
      G.execBool ghciProc outHdl condStr >>= withResult

    withResult (Left err) = do
      infoM _LOG_NAME err
      continueWithCondResult True
    withResult (Right condRes) = continueWithCondResult condRes

    outHdl msg = do
      sendStdoutEvent mvarCtx msg
      infoM _LOG_NAME msg

    forceBindings ghciProc outHdl = G.bindings ghciProc outHdl >>= \case
      Left err -> do
        errorM _LOG_NAME $ "[forceBindings] " ++ err
        sendErrorEvent mvarCtx $ "[forceBindings] " ++ err
        return ()
      Right bs -> do
        mapM_ (forceBind ghciProc outHdl . G.nameBindingData) bs
        return ()

    forceBind ghciProc outHdl name = G.force ghciProc outHdl name >>= \case
      Left err -> do
        errorM _LOG_NAME $ "[forceBindings] " ++ err
        sendErrorEvent mvarCtx $ "[forceBindings] " ++ err
        return ()
      Right _ -> return ()

    -- |
    --
    continueWithCondResult False = do
      sendConsoleEvent mvarCtx "[INFO] continueed because condition False."
      startDebug mvarCtx
    continueWithCondResult True  = do
      sendConsoleEvent mvarCtx "[INFO] stopped because condition not False."
      sendStopEvent

    -- |
    --
    sendStopEvent = do
      infoM _LOG_NAME $ show pos

      ctx <- takeMVar mvarCtx
      putMVar mvarCtx ctx{debugStoppedPosDebugContextData = Just pos}

      resSeq <- getIncreasedResponseSequence mvarCtx
      let stopEvt    = J.defaultStoppedEvent resSeq
          stopEvtStr = J.encode stopEvt
      sendEvent stopEvtStr

-}
