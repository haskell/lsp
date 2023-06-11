module Language.LSP.Protocol.Types.WatchKinds where

import           Language.LSP.Protocol.Internal.Types (WatchKind(..))
import           Language.LSP.Protocol.Types.LspEnum (toEnumBaseType, fromOpenEnumBaseType)
import           Data.Set (toList, Set)

containsCreate :: WatchKind -> Bool
containsCreate WatchKind_Create = True
containsCreate (WatchKind_Custom 3) = True
containsCreate (WatchKind_Custom 5) = True
containsCreate (WatchKind_Custom 7) = True
containsCreate _ = False

containsChange :: WatchKind -> Bool
containsChange WatchKind_Change = True
containsChange (WatchKind_Custom 3) = True
containsChange (WatchKind_Custom 6) = True
containsChange (WatchKind_Custom 7) = True
containsChange _ = False

containsDelete :: WatchKind -> Bool
containsDelete WatchKind_Delete = True
containsDelete (WatchKind_Custom 5) = True
containsDelete (WatchKind_Custom 6) = True
containsDelete (WatchKind_Custom 7) = True
containsDelete _ = False

combineWatchKinds :: Set WatchKind -> WatchKind
combineWatchKinds s = fromOpenEnumBaseType $ sum $ toEnumBaseType <$> toList s