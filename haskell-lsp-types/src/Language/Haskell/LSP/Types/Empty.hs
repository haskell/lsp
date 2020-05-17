module Language.Haskell.LSP.Types.Empty where

import Data.Aeson

data Empty = Empty deriving (Eq,Ord,Show)
instance ToJSON Empty where
  toJSON Empty = Null
instance FromJSON Empty where
  parseJSON Null = pure Empty
  parseJSON _ = mempty
