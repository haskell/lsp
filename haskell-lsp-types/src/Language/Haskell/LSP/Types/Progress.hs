module Language.Haskell.LSP.Types.Progress where

import qualified Data.Aeson as A
import Data.Text (Text)

-- | A token used to report progress back or return partial results for a
-- specific request.
-- @since 0.17.0.0
data ProgressToken
    = ProgressNumericToken Int
    | ProgressTextToken Text
    deriving (Show, Read, Eq, Ord)

instance A.ToJSON ProgressToken where
    toJSON (ProgressNumericToken i) = A.toJSON i
    toJSON (ProgressTextToken t) = A.toJSON t

instance A.FromJSON ProgressToken where
    parseJSON (A.String t) = pure $ ProgressTextToken t
    parseJSON (A.Number i) = ProgressNumericToken <$> A.parseJSON (A.Number i)
    parseJSON v = fail $ "Invalid progress token: " ++ show v
