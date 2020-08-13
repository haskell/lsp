{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase                 #-}
-- | Internal helpers for generating definitions
module Language.Haskell.LSP.Types.Utils
  ( rdrop
  , makeSingletonFromJSON
  , deriveJSONExtendFields
  , makeRegHelper
  , makeExtendingDatatype
  , lspOptions
  ) where

import Language.Haskell.TH
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.String
import Control.Monad
import Data.List (foldl', (\\))

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

-- | Generate ToJSON/FromJSON instances where the specified fields are
-- encoded directly inside object, unwrapped, and not another layer.
-- Use this for encoding types that extend an interface in the typescript
-- specification.
-- The field names are passed as strings to work around duplicate record fields.
deriveJSONExtendFields :: Options -> Name -> [String] -> Q [Dec]
deriveJSONExtendFields opts name fieldStringsToExtend = do
  TyConI datad <- reify name
  let DataD _ _ _ _ [con] _ = datad
      RecC conName varbangtyps = con
      fields = map (\(n,_,_) -> n) varbangtyps
      conType = ConT name

      fieldNames = map (\(n,_,_) -> n) varbangtyps
      lookupFields s =
        case filter ((== s) . nameBase) fieldNames of
          [n] -> pure n
          _ -> fail $ "Couldn't find field to extend: " <> s

  -- Need to convert from strings of fields -> names of fields
  fieldsToExtend <- mapM lookupFields fieldStringsToExtend

  to <- deriveToJSONExtendFields opts (pure conType) fields fieldsToExtend
  from <- deriveFromJSONExtendFields opts conType conName fields fieldsToExtend
  return (to ++ from)

{-
-- Note: in extends, we need to put the x there to disambiguate in the presence of
  duplicate record fields

instance ToJSON Foo where
  toJSON x = Object (mconcat (mainObj:extendMaps))
    where extends = map [toJSON (_baz (x :: Foo))]
          unwrapObj (Object hm) = hm
          extendMaps = map unwrapObj extends
          mainObj = HM.fromList [("_foo", toJSON (_foo (x :: Foo)))]
-}
deriveToJSONExtendFields :: Options -> TypeQ -> [Name] -> [Name] -> Q [Dec]
deriveToJSONExtendFields opts typ fields fieldsToExtend = do
  xName <- newName "x"
  let mkToJSON :: Name -> ExpQ
      mkToJSON n = [e| toJSON ($(varE n) ($(varE xName) :: $typ))|]
      mkHMTuple fieldName =
        [e| (fromString $(stringE (fieldLabelModifier opts (nameBase fieldName)))
          , toJSON ($(varE fieldName) ($(varE xName) :: $typ ))) |]

  [d| instance ToJSON $typ where
          toJSON $(varP xName) = Object (mconcat (mainObj:extendMaps))
            where extends = $(listE (mkToJSON <$> fieldsToExtend))
                  unwrapObj (Object hm) = hm
                  extendMaps = map unwrapObj extends
                  mainObj = HM.fromList
                    $(listE (mkHMTuple <$> (fields \\ fieldsToExtend)))
    |]
  where

{-
instance FromJSON Foo where
  parseJSON o@(Object v) =
    Foo <$> parseJSON o <*> v .: "foo"
  parseJSON _ = mempty
-}
deriveFromJSONExtendFields :: Options -> Type -> Name -> [Name] -> [Name] -> Q [Dec]
deriveFromJSONExtendFields opts typ tyConName fields fieldsToExtend = do
  oName <- newName "_o" -- the object name
  vName <- newName "_v" -- the value name
  ConE objectName <- [e| Object |]

  let fieldExprs = map mkParseExp fields
      pat = asP oName (conP objectName [varP vName])
      apArgs :: [ExpQ] -> ExpQ
      apArgs [] = error "No arguments!"
      apArgs [e] = e
      apArgs [e,e'] = [e| $e <$> $e' |]
      apArgs es = [e| $(apArgs (init es)) <*> $(last es) |]

      mkParseExp fieldName
        | fieldName `elem` fieldsToExtend = [e| parseJSON $(varE oName) |]
        | otherwise =
          [e| $(varE vName) .: fromString $(stringE (fieldLabelModifier opts (nameBase fieldName))) |]

  [d| instance FromJSON $(pure typ) where
        parseJSON $pat = $(apArgs ((conE tyConName):fieldExprs))
        parseJSON _ = mempty
    |]

makeRegHelper :: Name -> DecsQ
makeRegHelper regOptTypeName = do
  Just sMethodTypeName <- lookupTypeName "SMethod"
  Just fromClientName <- lookupValueName "FromClient"
  TyConI (DataD _ _ _ _ allCons _) <- reify sMethodTypeName

  let isConsFromClient (GadtC _ _ (AppT _ method)) = isMethodFromClient method
      isConsFromClient _ = return False
      isMethodFromClient :: Type -> Q Bool
      isMethodFromClient (PromotedT method) = do
        typ <- reifyType method
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

-- | Generates a datatype
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

