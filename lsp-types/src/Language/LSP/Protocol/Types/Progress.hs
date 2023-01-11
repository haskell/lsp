module Language.LSP.Protocol.Types.Progress
  ( _workDoneProgressBegin
  , _workDoneProgressEnd
  , _workDoneProgressReport
  )
  where

import           Control.Lens
import           Data.Aeson

import           Language.LSP.Protocol.Internal.Types.WorkDoneProgressBegin
import           Language.LSP.Protocol.Internal.Types.WorkDoneProgressEnd
import           Language.LSP.Protocol.Internal.Types.WorkDoneProgressReport

-- From lens-aeson
_JSON :: (ToJSON a, FromJSON a) => Prism' Value a
_JSON = prism toJSON $ \x -> case fromJSON x of
    Success y -> Right y;
    _         -> Left x

-- | Prism for extracting the 'WorkDoneProgressBegin' case from the unstructured 'value' field of 'ProgressParams'.
_workDoneProgressBegin :: Prism' Value WorkDoneProgressBegin
_workDoneProgressBegin = _JSON

-- | Prism for extracting the 'WorkDoneProgressEnd' case from the unstructured 'value' field of 'ProgressParams'.
_workDoneProgressEnd :: Prism' Value WorkDoneProgressEnd
_workDoneProgressEnd = _JSON

-- | Prism for extracting the 'WorkDoneProgressReport' case from the unstructured 'value' field of 'ProgressParams'.
_workDoneProgressReport :: Prism' Value WorkDoneProgressReport
_workDoneProgressReport = _JSON
