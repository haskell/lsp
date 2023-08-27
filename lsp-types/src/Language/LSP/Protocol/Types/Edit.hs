{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Language.LSP.Protocol.Types.Edit where

import Data.Text (Text)
import Data.Text qualified as T

import Control.Lens hiding (index)
import Language.LSP.Protocol.Internal.Types
import Language.LSP.Protocol.Types.Common

-- | Convenience alias for the type in the 'WorkspaceEdit._documentChanges' field.
type DocumentChange = TextDocumentEdit |? CreateFile |? RenameFile |? DeleteFile

-- TODO: get rid of this in favour of the more correct things in VFS

{- | Applies a 'TextEdit' to some 'Text'.

 >>> applyTextEdit (TextEdit (Range (Position 0 1) (Position 0 2)) "i") "foo"
 "fio"
-}
applyTextEdit :: TextEdit -> Text -> Text
applyTextEdit (TextEdit (Range sp ep) newText) oldText =
  let (_, afterEnd) = splitAtPos ep oldText
      (beforeStart, _) = splitAtPos sp oldText
   in mconcat [beforeStart, newText, afterEnd]
 where
  splitAtPos :: Position -> Text -> (Text, Text)
  splitAtPos (Position sl sc) t =
    -- If we are looking for a line beyond the end of the text, this will give us an index
    -- past the end. Fortunately, T.splitAt is fine with this, and just gives us the whole
    -- string and an empty string, which is what we want.
    let index = sc + startLineIndex sl t
     in T.splitAt (fromIntegral index) t

  -- The index of the first character of line 'line'
  startLineIndex :: UInt -> Text -> UInt
  startLineIndex 0 _ = 0
  startLineIndex line t' =
    case T.findIndex (== '\n') t' of
      Just i -> fromIntegral i + 1 + startLineIndex (line - 1) (T.drop (i + 1) t')
      -- i != 0, and there are no newlines, so this is a line beyond the end of the text.
      -- In this case give the "start index" as the end, so we will at least append the text.
      Nothing -> fromIntegral $ T.length t'

-- | 'editTextEdit' @outer@ @inner@ applies @inner@ to the text inside @outer@.
editTextEdit :: TextEdit -> TextEdit -> TextEdit
editTextEdit (TextEdit origRange origText) innerEdit =
  let newText = applyTextEdit innerEdit origText
   in TextEdit origRange newText

-- | Conversion between 'OptionalVersionedTextDocumentIdentifier' and 'VersionedTextDocumentIdentifier'.
_versionedTextDocumentIdentifier :: Prism' OptionalVersionedTextDocumentIdentifier VersionedTextDocumentIdentifier
_versionedTextDocumentIdentifier = prism down up
 where
  down (VersionedTextDocumentIdentifier uri v) = OptionalVersionedTextDocumentIdentifier uri (InL v)
  up (OptionalVersionedTextDocumentIdentifier uri (InL v)) = Right $ VersionedTextDocumentIdentifier uri v
  up i@(OptionalVersionedTextDocumentIdentifier _ (InR _)) = Left i
