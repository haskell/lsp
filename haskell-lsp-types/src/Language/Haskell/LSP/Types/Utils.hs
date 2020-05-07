module Language.Haskell.LSP.Types.Utils where

import Language.Haskell.TH
import Control.Monad.IO.Class

-- ---------------------------------------------------------------------

rdrop :: Int -> [a] -> [a]
rdrop cnt = reverse . drop cnt . reverse

makeSingletonInstances :: Name -> Q [Dec]
makeSingletonInstances n = do
  TyConI (DataD _ _ _ _ cons _)<- reify n
  concat <$> mapM makeInst cons

makeInst :: Con -> Q [Dec]
makeInst (GadtC [singleton] _ t) = do
  AppT (ConT smethod) promotedmethod@(PromotedT tn) <- pure t
  Just fj <- lookupTypeName "FromJSON"
  Just pj <- lookupValueName "parseJSON"
  Just sm <- lookupValueName "SomeMethod"
  Just mp <- lookupValueName "mempty"
  Just pu <- lookupValueName "pure"
  v <- newName "v"
  x <- newName "x"
  let inst = InstanceD Nothing [] (AppT (ConT fj) t) [val]
      val = FunD pj [clause]
      clause = Clause [VarP v] (NormalB exp) []
      exp = DoE [BindS (VarP x) (AppE (VarE pj) (VarE v)),NoBindS cs]
      cs = CaseE (VarE x) [c1,c2]
      c1 = Match (ConP sm [ConP singleton []]) (NormalB (AppE (VarE pu) (ConE singleton))) []
      c2 = Match WildP (NormalB (VarE mp)) []
  liftIO $ print inst
  pure [inst]
makeInst _ = pure []

{-
instance A.FromJSON (SMethod Initialized) where
  parseJSON x = do
    m <- parseJSON x
    case m of
      SomeMethod SInitialized -> pure $ SInitialized
      _ -> mempty
-}
