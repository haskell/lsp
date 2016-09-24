module Language.Haskell.LSP.Constant where

-- |
--
--
_INI_SEC_LOG :: String
_INI_SEC_LOG   = "LOG"

_INI_LOG_FILE :: String
_INI_LOG_FILE  = "file"

_INI_LOG_LEVEL :: String
_INI_LOG_LEVEL = "level"

_INI_SEC_PHOITYNE :: String
_INI_SEC_PHOITYNE = "PHOITYNE"


-- |
--
--
_LOG_NAME :: String
_LOG_NAME = "phoityne"

_LOG_FORMAT :: String
_LOG_FORMAT = "$time [$tid] $prio $loggername - $msg"

_LOG_FORMAT_DATE :: String
_LOG_FORMAT_DATE = "%Y-%m-%d %H:%M:%S"


-- |
--
--
type ModuleName = String

_HS_FILE_EXT :: String
_HS_FILE_EXT = ".hs"

_PHOITYNE_GHCI_PROMPT :: String
_PHOITYNE_GHCI_PROMPT = "Phoityne>>= "

_GHCI_PROMPT :: String
_GHCI_PROMPT = "Prelude> "

_GHCI_MAIN_PROMPT :: String
_GHCI_MAIN_PROMPT = "*Main> "

_PROJECT_ROOT_MODULE_NAME :: String
_PROJECT_ROOT_MODULE_NAME = "Project Root"

_INDENT_SPACE :: String
_INDENT_SPACE = "  "

_COMMENT_TAG :: String
_COMMENT_TAG = "-- "

_UNDO_BUFFER_MAX_SIZE :: Int
_UNDO_BUFFER_MAX_SIZE = 100

_AVAILABLE_FILE_EXT :: [String]
_AVAILABLE_FILE_EXT = [ ".hs"
                      , ".ini"
                      , ".yaml"
                      , ".cabal"
                      ]

-- |
--
--
_NO_BREAK_POINT_LOCATION :: String
_NO_BREAK_POINT_LOCATION = "No breakpoints found at that location."

