module Language.LSP.Protocol.Types.Progress (workDoneProgressBegin, workDoneProgressEnd, workDoneProgressReport) where

import Data.Aeson
import Language.LSP.Protocol.Internal.Types
import Language.LSP.Protocol.Utils.Misc (Prism, prism)

-- From lens-aeson
_JSON :: (ToJSON a, FromJSON a) => Prism Value Value a a
_JSON = prism toJSON $ \v -> case fromJSON v of
  Success y -> Right y
  _ -> Left v

-- | Prism for extracting the 'WorkDoneProgressBegin' case from the unstructured 'value' field of 'ProgressParams'.
workDoneProgressBegin :: Prism Value Value WorkDoneProgressBegin WorkDoneProgressBegin
workDoneProgressBegin = _JSON

-- | Prism for extracting the 'WorkDoneProgressEnd' case from the unstructured 'value' field of 'ProgressParams'.
workDoneProgressEnd :: Prism Value Value WorkDoneProgressEnd WorkDoneProgressEnd
workDoneProgressEnd = _JSON

-- | Prism for extracting the 'WorkDoneProgressReport' case from the unstructured 'value' field of 'ProgressParams'.
workDoneProgressReport :: Prism Value Value WorkDoneProgressReport WorkDoneProgressReport
workDoneProgressReport = _JSON
