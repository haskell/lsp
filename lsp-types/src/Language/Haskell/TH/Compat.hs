{-# LANGUAGE CPP #-}
module Language.Haskell.TH.Compat
  ( addHaddock
  ) where

import           Data.Text           (Text)
import qualified Language.Haskell.TH as TH
#if MIN_VERSION_template_haskell(2,18,0)
import qualified Data.Text as T
import Data.Functor (void)
#endif

-- | Compatibility wrapper for the documentation inclusion facility
-- added in `template-haskell-2.18`.
addHaddock :: TH.Name -> Text -> TH.Q ()
addHaddock nm doc =
#if MIN_VERSION_template_haskell(2,18,0)
  void $ TH.putDoc (TH.DeclDoc nm) (T.unpack doc)
#else
  pure ()
#endif
