{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase                 #-}
module Language.Haskell.LSP.Types.Utils
  ( rdrop
  , makeSingletonFromJSON
  ) where

import Language.Haskell.TH
import Data.Aeson
import Control.Monad
import Data.List (foldl')

-- ---------------------------------------------------------------------

rdrop :: Int -> [a] -> [a]
rdrop cnt = reverse . drop cnt . reverse

-- | Given a wrapper and a singleton GADT, construct FromJSON
-- instances for each constructor return type by invoking the
-- FromJSON instance for the wrapper and unwrapping
makeSingletonFromJSON :: Name -> Name -> Q [Dec]
makeSingletonFromJSON wrap gadt = do
  TyConI (DataD _ _ _ _ cons _) <- reify gadt
  concat <$> mapM (makeInst wrap) cons

{-
instance FromJSON (SMethod $method) where
  parseJSON = parseJSON >=> \case
      SomeMethod $singleton-method -> pure $singleton-method
      _ -> mempty
-}
makeInst :: Name -> Con -> Q [Dec]
makeInst wrap (GadtC [sConstructor] args t) = do
  ns <- replicateM (length args) (newName "x")
  let wrappedPat = pure $ ConP   wrap [ConP sConstructor  (map VarP ns)]
      unwrappedE = pure $ foldl' AppE (ConE sConstructor) (map VarE ns)
  [d| instance FromJSON $(pure t) where
        parseJSON = parseJSON >=> \case
          $wrappedPat -> pure $unwrappedE
          _ -> mempty
    |]
makeInst wrap (ForallC _ _ con) = makeInst wrap con -- Cancel and Custom requests
makeInst _ _ = fail "makeInst only defined for GADT constructors"
