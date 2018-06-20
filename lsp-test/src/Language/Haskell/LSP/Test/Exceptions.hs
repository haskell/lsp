module Language.Haskell.LSP.Test.Exceptions where

import Control.Exception

data SessionException = TimeoutException
  deriving Show
instance Exception SessionException

anySessionException :: SessionException -> Bool
anySessionException = const True