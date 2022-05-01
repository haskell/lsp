{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Internal helpers for generating definitions
module Language.LSP.Types.Utils
  ( rdrop
  , makeSingletonFromJSON
  , makeRegHelper
  , lspOptions
  , lspOptionsUntagged
  ) where

import           Control.Monad
import           Data.Aeson
import           Data.List
import           Data.Maybe          (mapMaybe)
import           Language.Haskell.TH

-- ---------------------------------------------------------------------

rdrop :: Int -> [a] -> [a]
rdrop cnt = reverse . drop cnt . reverse

-- | Given a wrapper and a singleton GADT, construct FromJSON
-- instances for each constructor return type by invoking the
-- FromJSON instance for the wrapper and unwrapping
makeSingletonFromJSON :: Name -> Name -> [Name] -> Q [Dec]
makeSingletonFromJSON wrap gadt skip = do
  TyConI (DataD _ _ _ _ cons _) <- reify gadt
  concat <$> (sequence $ mapMaybe (makeInst wrap skip) cons)

{-
instance FromJSON (SMethod $method) where
  parseJSON = parseJSON >=> \case
      SomeMethod $singleton-method -> pure $singleton-method
      _ -> mempty
-}
makeInst :: Name -> [Name] -> Con -> Maybe (Q [Dec])
makeInst _ skip (GadtC [sConstructor] _ _) | sConstructor `elem` skip = Nothing
makeInst wrap _ (GadtC [sConstructor] args t) = Just $ do
  ns <- replicateM (length args) (newName "x")
  let wrappedPat = conP wrap [conP sConstructor (map varP ns)]
      unwrappedE = pure $ foldl' AppE (ConE sConstructor) (map VarE ns)
  [d| instance FromJSON $(pure t) where
        parseJSON = parseJSON >=> \case
          $wrappedPat -> pure $unwrappedE
          _           -> mempty
    |]
makeInst wrap skip (ForallC _ _ con) = makeInst wrap skip con -- Cancel and Custom requests
makeInst _ _ _ = Just $ fail "makeInst only defined for GADT constructors"

makeRegHelper :: Name -> DecsQ
makeRegHelper regOptTypeName = do
  Just sMethodTypeName <- lookupTypeName "SMethod"
  Just fromClientName <- lookupValueName "ClientToServer"
  TyConI (DataD _ _ _ _ allCons _) <- reify sMethodTypeName

  let isConsFromClient (GadtC _ _ (AppT _ method)) = isMethodFromClient method
      isConsFromClient _                           = return False
      isMethodFromClient :: Type -> Q Bool
      isMethodFromClient (PromotedT method) = do
        DataConI _ typ _ <- reify method
        case typ of
          AppT (AppT _ (PromotedT n)) _ -> return $ n == fromClientName
          _                             -> return False
      isMethodFromClient _ = fail "Didn't expect this type of Method!"

  cons <- filterM isConsFromClient allCons

  let conNames = map (\(GadtC [name] _ _) -> name) cons
      helperName = mkName "regHelper"
      mkClause name = do
        x <- newName "x"
        clause [ conP name [], varP x ]
               (normalB (varE x))
               []
      regOptTcon = conT regOptTypeName
  fun <- funD helperName (map mkClause conNames)

  typSig <- sigD helperName $
    [t| forall m x. $(conT sMethodTypeName) m
        -> (Show ($regOptTcon m) => ToJSON ($regOptTcon m) => FromJSON ($regOptTcon m) => x)
        -> x |]
  return [typSig, fun]

-- | Standard options for use when generating JSON instances
-- NOTE: This needs to be in a separate file because of the TH stage restriction
lspOptions :: Options
lspOptions = defaultOptions { omitNothingFields = True, fieldLabelModifier = modifier }
  where
  modifier :: String -> String
  -- For fields called data and type in the spec, we call them xdata and xtype
  -- in haskell-lsp-types to avoid it clashing with the Haskell keywords. This
  -- fixes up the json derivation
  modifier "_xdata" = "data"
  modifier "_xtype" = "type"
  modifier xs       = drop 1 xs

-- | Standard options for use when generating JSON instances for an untagged union
lspOptionsUntagged :: Options
lspOptionsUntagged = lspOptions { sumEncoding = UntaggedValue }

