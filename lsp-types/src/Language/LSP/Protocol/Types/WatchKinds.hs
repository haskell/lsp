module Language.LSP.Protocol.Types.WatchKinds where

import Data.Set (Set, toList)
import Language.LSP.Protocol.Internal.Types (WatchKind (..))
import Language.LSP.Protocol.Types.LspEnum (fromOpenEnumBaseType, toEnumBaseType)

-- WatchKind is better represented as a Set than as enum. As the lsp spec
-- defines them as an enum, these helper functions help bridge the difference.

-- | Tests whether `WatchKind_Create` is contained in the provided WatchKind enum
containsCreate :: WatchKind -> Bool
containsCreate WatchKind_Create = True
containsCreate (WatchKind_Custom 3) = True
containsCreate (WatchKind_Custom 5) = True
containsCreate (WatchKind_Custom 7) = True
containsCreate _ = False

-- | Tests whether `WatchKind_Change` is contained in the provided WatchKind enum
containsChange :: WatchKind -> Bool
containsChange WatchKind_Change = True
containsChange (WatchKind_Custom 3) = True
containsChange (WatchKind_Custom 6) = True
containsChange (WatchKind_Custom 7) = True
containsChange _ = False

-- | Tests whether `WatchKind_Delete` is contained in the provided WatchKind enum
containsDelete :: WatchKind -> Bool
containsDelete WatchKind_Delete = True
containsDelete (WatchKind_Custom 5) = True
containsDelete (WatchKind_Custom 6) = True
containsDelete (WatchKind_Custom 7) = True
containsDelete _ = False

{- | Combine a set of WatchKind types into a new WatchKind type that accurately
 represents the set
-}
combineWatchKinds :: Set WatchKind -> WatchKind
combineWatchKinds s = fromOpenEnumBaseType $ sum $ toEnumBaseType <$> toList s
