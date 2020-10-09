{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase                 #-}
-- | Internal helpers for generating definitions
module Language.Haskell.LSP.Types.Utils
  ( rdrop
  , makeSingletonFromJSON
  , makeRegHelper
  , makeExtendingDatatype
  , lspOptions
  ) where

import Control.Monad
import Data.Aeson
import Data.List
import Language.Haskell.TH

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

makeRegHelper :: Name -> DecsQ
makeRegHelper regOptTypeName = do
  Just sMethodTypeName <- lookupTypeName "SMethod"
  Just fromClientName <- lookupValueName "FromClient"
  TyConI (DataD _ _ _ _ allCons _) <- reify sMethodTypeName

  let isConsFromClient (GadtC _ _ (AppT _ method)) = isMethodFromClient method
      isConsFromClient _ = return False
      isMethodFromClient :: Type -> Q Bool
      isMethodFromClient (PromotedT method) = do
        DataConI _ typ _ <- reify method
        case typ of
          AppT (AppT _ (PromotedT n)) _ -> return $ n == fromClientName
          _ -> return False
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

-- | @makeExtendingDatatype name extends fields@ generates a record datatype
-- that contains all the fields of @extends@, plus the additional fields in
-- @fields@.
-- e.g.
-- data Foo = { a :: Int }
-- makeExtendingDatatype "bar" [''Foo] [("b", [t| String |])]
-- Will generate
-- data Bar = { a :: Int, b :: String }
makeExtendingDatatype :: String -> [Name] -> [(String, TypeQ)] -> DecsQ
makeExtendingDatatype datatypeNameStr extends fields = do
  extendFields <- fmap concat $ forM extends $ \e -> do
    TyConI (DataD _ _ _ _ [RecC _ eFields] _) <- reify e
    return eFields
  let datatypeName = mkName datatypeNameStr
      insts = [[t| Read |], [t| Show |], [t| Eq |]]
      constructor = recC datatypeName combinedFields
      userFields = flip map fields $ \(s, typ) -> do
        varBangType (mkName s) (bangType (bang noSourceUnpackedness noSourceStrictness) typ)
      combinedFields = (map pure extendFields) <> userFields
      derivs = [derivClause Nothing insts]
  (\a -> [a]) <$> dataD (cxt []) datatypeName [] Nothing [constructor] derivs

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
  modifier xs = drop 1 xs

