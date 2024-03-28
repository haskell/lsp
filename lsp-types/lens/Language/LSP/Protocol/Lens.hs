-- | Lenses for the LSP types that can't easily be derived with @generic-lens@ or similar.
module Language.LSP.Protocol.Lens (
  versionedTextDocumentIdentifier,
  workDoneProgressBegin,
  workDoneProgressEnd,
  workDoneProgressReport,
)
where

import Control.Lens
import Data.Aeson

import Language.LSP.Protocol.Types

-- From lens-aeson
_JSON :: (ToJSON a, FromJSON a) => Prism' Value a
_JSON = prism toJSON $ \x -> case fromJSON x of
  Success y -> Right y
  _ -> Left x

-- | Prism for extracting the 'WorkDoneProgressBegin' case from the unstructured 'value' field of 'ProgressParams'.
workDoneProgressBegin :: Prism' Value WorkDoneProgressBegin
workDoneProgressBegin = _JSON

-- | Prism for extracting the 'WorkDoneProgressEnd' case from the unstructured 'value' field of 'ProgressParams'.
workDoneProgressEnd :: Prism' Value WorkDoneProgressEnd
workDoneProgressEnd = _JSON

-- | Prism for extracting the 'WorkDoneProgressReport' case from the unstructured 'value' field of 'ProgressParams'.
workDoneProgressReport :: Prism' Value WorkDoneProgressReport
workDoneProgressReport = _JSON

-- | Conversion between 'OptionalVersionedTextDocumentIdentifier' and 'VersionedTextDocumentIdentifier'.
versionedTextDocumentIdentifier :: Prism' OptionalVersionedTextDocumentIdentifier VersionedTextDocumentIdentifier
versionedTextDocumentIdentifier = prism down up
 where
  down (VersionedTextDocumentIdentifier uri v) = OptionalVersionedTextDocumentIdentifier uri (InL v)
  up (OptionalVersionedTextDocumentIdentifier uri (InL v)) = Right $ VersionedTextDocumentIdentifier uri v
  up i@(OptionalVersionedTextDocumentIdentifier _ (InR _)) = Left i
