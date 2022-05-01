{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ViewPatterns             #-}
{-|
Code generation of Haskell types and functions based on the LSP metamodel.
-}
module Language.LSP.MetaModel.CodeGen where

import           Language.LSP.MetaModel.Types  hiding (MessageDirection (..))
import qualified Language.LSP.MetaModel.Types  as MM
import           Language.LSP.Types.Common     hiding (MessageKind (..),
                                                Null (..))
import qualified Language.LSP.Types.Common     as C
import qualified Language.LSP.Types.Uri        as Uri

import           Control.Lens.Internal.FieldTH
import           Control.Lens.TH
import           Control.Monad.Reader
import           Control.Monad.State           (evalStateT)
import qualified Data.Aeson                    as J
import           Data.Foldable                 (foldl')
import qualified Data.Kind                     as Kind
import qualified Data.Map                      as Map
import           Data.Maybe                    (catMaybes, fromMaybe, mapMaybe,
                                                maybeToList)
import           Data.Proxy
import           Data.Row
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Traversable
import           Data.Void
import qualified GHC.TypeLits                  as TL
import qualified Language.Haskell.TH           as TH
import qualified Language.Haskell.TH.Syntax    as TH
import           Language.Haskell.TH.Compat

{-
TODO: types are all lazy now, is that right?
-}

-- | A mapping from names in the metamodel to their names in the generated Haskell.
type SymbolTable = Map.Map Text TH.Name

-- | A mapping from names in the metamodel to their structure definition, used for chasing
-- supertypes.
type StructTable = Map.Map Text Structure

data CodeGenEnv = CodeGenEnv {
  symbolTable :: SymbolTable
  , structTable :: StructTable
  }
type CodeGenM = ReaderT CodeGenEnv TH.Q

-- | Given the metamodel, make a splice with declarations for all the types.
genMetaModel :: MetaModel -> TH.Q [ TH.Dec ]
genMetaModel mm = do
  (symbolTable, structTable) <- buildTables mm
  flip runReaderT (CodeGenEnv symbolTable structTable) $ do
    structs <- concat <$> traverse genStruct (structures mm)
    -- Make a binding that just lists all the struct names so we can use them later
    -- See Note [Generating lenses]
    structNames <- do
      -- Have to use the string form of the generated Name
      -- since we might have mangled the original name
      let structNames = mapMaybe (\Structure{name} -> Map.lookup name symbolTable) (structures mm)
      structNameEs <- lift $ traverse (TH.lift . TH.nameBase) structNames
      lift
        [d|
          structNames :: [String]
          structNames = $(pure $ TH.ListE structNameEs)
        |]
    enums <- concat <$> traverse genEnumeration (enumerations mm)
    aliases <- traverse genAlias (typeAliases mm)
    methods <- genMethods (requests mm) (notifications mm)
    pure (structs ++ structNames ++ enums ++ aliases ++ methods)

  -- | Convenience wrapper around 'genMetaModel' that reads the input from a file.
genMetaModelFromFile :: FilePath -> TH.Q [ TH.Dec ]
genMetaModelFromFile fp = do
  TH.addDependentFile fp
  res <- liftIO $ J.eitherDecodeFileStrict' fp
  case res of
    Left e   -> fail e
    Right mm -> genMetaModel mm

-- | Given a list of type names, make a splice that generates the lens typeclass declarations
-- for all of them. Defined here to avoid stage restrictions.
genLenses :: [String] -> TH.Q [TH.Dec]
genLenses structNames = do
  let structNames' = fmap TH.mkName structNames
  let
    -- We need to use the internals of the lens TH machinery so that we can do this
    -- in one go without generating duplicate classes.
    opticMaker :: TH.Name -> HasFieldClasses [TH.Dec]
    opticMaker n = do
        (TH.TyConI d) <- lift $ TH.reify n
        makeFieldOpticsForDec' classUnderscoreNoPrefixFields d
  decss <- flip evalStateT mempty $ traverse opticMaker structNames'
  pure $ concat decss

------------

-- | Names we can't put in Haskell code.
reservedNames :: Set.Set T.Text
reservedNames = Set.fromList [ "data", "type" ]

-- | Sanitize a name so we can use it in Haskell.
sanitizeName :: T.Text -> T.Text
sanitizeName n =
  -- Names can't start with underscores! Replace that with a 'U' for lack
  -- of a better idea
  let n' = if "_" `T.isPrefixOf` n then T.cons 'U' $ T.tail n else n
  -- Names can't have '$'s! Just throw them away.
      n'' = T.filter (\c -> c /= '$') n'
  -- If we end up with a reserved name, suffix with an underscore. This
  -- relibly gets us something recognizable, rather than trying to systematize
  -- the conversion of 'type' into 'tpe' or similar.
      n''' = if n'' `Set.member` reservedNames then n'' <> "_" else n''
  in n'''

-- | Make a name to be used at the top-level (i.e. not as a member of anything).
makeToplevelName :: T.Text -> TH.Q TH.Name
makeToplevelName n = TH.newName (T.unpack $ capitalize $ sanitizeName n)

-- | Make a name for a constructor, optionally including a contextual name to qualify it with.
makeConstrName :: Maybe T.Text -> T.Text -> CodeGenM TH.Name
makeConstrName context n = do
  let
    cap = capitalize n
    disambiguated = case context of { Just t -> t <> "_" <> cap; Nothing -> cap }
  pure $ TH.mkName $ T.unpack $ sanitizeName disambiguated

-- | Make a name for a field.
makeFieldName :: T.Text -> CodeGenM TH.Name
makeFieldName n = do
  let
    prefixed = "_" <> sanitizeName n
  pure $ TH.mkName $ T.unpack prefixed

-- | Build the tables we need for processing the metamodel.
buildTables :: MetaModel -> TH.Q (SymbolTable, StructTable)
buildTables (MetaModel{structures, enumerations, typeAliases}) = do
  bothEntries <- for structures $ \s@Structure{name} -> do
    thName <- makeToplevelName name
    pure ((name, thName), (name, s))
  let (entries, sentries) = unzip bothEntries

  entries' <- for enumerations $ \Enumeration{name} -> do
    thName <- makeToplevelName name
    pure (name, thName)

  entries'' <- for typeAliases $ \TypeAlias{name} -> do
    thName <- makeToplevelName name
    pure (name, thName)

  let symbolTable = Map.fromList $ entries <> entries' <> entries''
      structTable = Map.fromList sentries

  pure (symbolTable, structTable)

--------

-- | Translate a type in the metamodel into the corresponding Haskell type.
-- See Note [Translating metamodel types]
convertType :: Type -> CodeGenM TH.Type
convertType = \case
  BaseType n -> case n of
    URI         -> lift [t| Uri.Uri |]
    DocumentUri -> lift [t| Uri.Uri |]
    Integer     -> lift [t| Int32 |]
    UInteger    -> lift [t| UInt |]
    Decimal     -> lift [t| Float |]
    RegExp      -> lift [t| Text |]
    String      -> lift [t| Text |]
    Boolean     -> lift [t| Bool |]
    Null        -> lift [t| C.Null |]

  -- Special cases: these are in fact defined in the meta model, but
  -- we have way better types for them

  -- 'LSPAny' is a big union of anything in the metamodel, we just
  -- keep that as an aeson 'Value'
  ReferenceType "LSPAny" -> lift [t| J.Value |]
  -- 'LSPObject' is an empty structure ... better to just say it's an aeson 'Object'!
  ReferenceType "LSPObject" -> lift [t| J.Object |]
  -- 'LSPArray' is a list of 'LSPAny'... better to just say it's an aeson 'Array'!
  ReferenceType "LSPArray" -> lift [t| J.Array |]

  ReferenceType n -> do
    st <- asks symbolTable
    case Map.lookup n st of
      Just thn -> pure $ TH.ConT thn
      Nothing  -> fail $ "Reference to unknown type: " <> show n
  ArrayType e -> TH.AppT TH.ListT <$> convertType e
  MapType k v -> do
    kt <- convertType k
    vt <- convertType v
    pure $ mkIterAppT (TH.ConT ''Map.Map) [ kt, vt ]
  OrType es -> do
    est <- traverse convertType es
    pure $ foldr1 (\ty o -> TH.ConT ''(|?) `TH.AppT` ty `TH.AppT` o) est
  AndType es -> do
    st <- asks structTable
    props <- for es $ \case
      ReferenceType t | Just e <- Map.lookup t st -> getStructProperties e
      t -> fail $ "element of 'and' type was not a reference to a structure: " ++ show t
    genAnonymousStruct $ concat props
  StructureLiteralType (StructureLiteral {properties}) -> genAnonymousStruct properties
  TupleType es -> do
    est <- traverse convertType es
    pure $ mkIterAppT (TH.TupleT (length est)) est
  StringLiteralType s -> lift [t| AString $(TH.litT $ TH.strTyLit $ T.unpack s) |]
  IntegerLiteralType n -> lift [t| AnInteger $(TH.litT $ TH.numTyLit n) |]
  -- TODO: support these. They're not used in the metamodel currently
  BooleanLiteralType _ -> fail "unsupported: boolean literal types"

------------

-- | Generate a type declaration corresponding to an enumeration.
genEnumeration :: Enumeration -> CodeGenM [TH.Dec]
genEnumeration Enumeration{name, type_, values, supportsCustomValues, documentation, since, proposed} = do
  let enumName = name
      enumNameString = T.unpack enumName
      -- This indicates whether or not the enum is "open" and supports custom values.
      -- We need to branch on this a lot!
      custom = fromMaybe False supportsCustomValues

  st <- asks symbolTable
  tn <- case Map.lookup name st of
    Just n  -> pure n
    Nothing -> fail $ "no name for " ++ show name
  addDocumentation tn documentation since proposed

  -- The (Haskell) type of the elements of this enum. Useful, so we can generate various
  -- code (e.g. for parsing JSON) generically but use this type to pin down what we want to do.
  ty <- case type_ of
    BaseType Integer  -> lift [t| Int32 |]
    BaseType UInteger -> lift [t| UInt |]
    BaseType String   -> lift [t| Text |]
    _                 -> fail $ "enumeration of unexpected type " ++ show type_

  -- https://github.com/microsoft/vscode-languageserver-node/issues/1035
  let badEnumValues = ["jsonrpcReservedErrorRangeStart", "jsonrpcReservedErrorRangeEnd", "serverErrorStart", "serverErrorEnd"]
      values' = filter (\EnumerationEntry{name} -> not $ name `elem` badEnumValues) values
  -- The associations between constructor names and their literals
  assocs <- for values' $ \EnumerationEntry{name, value, documentation, since, proposed} -> do
    cn <- makeConstrName (Just enumName) name
    addDocumentation cn documentation since proposed
    let
      -- The literal for the actual enum value in this case
      lit = case value of
          T t -> TH.StringL $ T.unpack t
          I i -> TH.IntegerL i
    pure (cn, lit)

  let normalCons = fmap (\(cn, _) -> TH.NormalC cn []) assocs
  customCon <-
    if custom
    then do
      cn <- makeConstrName (Just enumName) "Custom"
      lift $ addHaddock cn "Custom values not defined in the LSP specification."
      pure $ Just $ TH.NormalC cn [(noBang, ty)]
    else pure Nothing
  let cons = normalCons ++ maybeToList customCon

  let datad = TH.DataD [] tn [] Nothing cons [stockDeriving]

  -- Generate functions for converting between the enum and its base representation. These
  -- are handy for defining e.g. JSON conversions simply.
  -- TODO: could do this with a class? Unclear if that's nicer or not
  -- xToValue :: X -> Value
  toBaseTypeN <- lift $ TH.newName $ T.unpack $ uncapitalize (sanitizeName enumName) <> "ToValue"
  toBaseTypeD <- do
    -- xToValue X1 = <X1 value>
    let normalClauses = (flip fmap) assocs $ \(n, v) -> TH.Clause [TH.ConP n []] (TH.NormalB $ TH.LitE v) []
    -- xToValue (CustomX c) = c
    customClause <- case customCon of
          Just (TH.NormalC cn _) -> do
            vn <- lift $ TH.newName "n"
            pure $ Just $ TH.Clause [TH.ConP cn [TH.VarP vn]] (TH.NormalB (TH.VarE vn)) []
          Just _ -> fail "impossible: wrong custom constructor"
          Nothing -> pure Nothing
    let clauses = normalClauses ++ maybeToList customClause
        sd = TH.SigD toBaseTypeN (TH.ArrowT `TH.AppT` TH.ConT tn `TH.AppT` ty)
        fd = TH.FunD toBaseTypeN clauses
    pure [ sd, fd ]

  -- Result type is Maybe unless X allows custom values, in which case we can always interpret
  -- a base value as a custom member
  -- valueToX :: <base type> -> Maybe X
  -- valueToX :: <base type> -> X
  fromBaseTypeN <- lift $ TH.newName $ T.unpack $ "valueTo" <> capitalize (sanitizeName enumName)
  fromBaseTypeD <- do
    -- valueToX <X1 value> = X
    -- or
    -- valueToX <X1 value> = Just X
    let normalClauses = (flip fmap) assocs $ \(n, v) -> TH.Clause [TH.LitP v] (TH.NormalB $ if custom then TH.ConE n else TH.VarE 'pure `TH.AppE` TH.ConE n) []
    -- valueToX c = CustomX c
    -- or
    -- valueToX _ = Nothing
    fallThroughClause <- case customCon of
        Just (TH.NormalC cn _) -> do
          n <- lift $ TH.newName "n"
          pure $ TH.Clause [TH.VarP n] (TH.NormalB (TH.ConE cn `TH.AppE` TH.VarE n)) []
        Just _ -> fail "impossible: wrong custom constructor"
        Nothing -> pure $ TH.Clause [TH.WildP] (TH.NormalB (TH.ConE 'Nothing)) []
    let clauses = normalClauses ++ [fallThroughClause]
        sd = TH.SigD fromBaseTypeN (TH.ArrowT `TH.AppT` ty `TH.AppT` (if custom then TH.ConT tn else TH.ConT ''Maybe `TH.AppT` TH.ConT tn))
        fd = TH.FunD fromBaseTypeN clauses
    pure [sd, fd]

  -- JSON instances in terms of the conversion functions. These are straighftorward, but still good to generate since there are so many!
  jsonDs <- lift
    [d|
     instance J.ToJSON $(TH.conT tn) where
       toJSON e = J.toJSON ($(TH.varE toBaseTypeN) e)

     instance J.FromJSON $(TH.conT tn) where
       parseJSON val = do
         v <- J.parseJSON val
         $(
           if custom
           then [| pure $ $(TH.varE fromBaseTypeN) v |]
           else [| case $(TH.varE fromBaseTypeN) v of { Just e -> pure e; Nothing -> fail $ "unrecognized enum value for " ++ enumNameString ++ ":" ++ show v; } |]
          )
      |]

  pure $ [datad] ++ toBaseTypeD ++ fromBaseTypeD ++ jsonDs

-- | Generate a type declaration corresponding to a type alias.
genAlias :: TypeAlias -> CodeGenM TH.Dec
genAlias TypeAlias{name, type_, documentation, since, proposed} = do
  st <- asks symbolTable
  tn <- case Map.lookup name st of
    Just n  -> pure n
    Nothing -> fail $ "no name for " ++ show name
  addDocumentation tn documentation since proposed

  rhs <- convertType type_

  ctor <- makeConstrName Nothing name
  -- We don't actually need JSONKey instances for all the base types, but it's close enough
  let jsonKey = case type_ of { BaseType _ -> True; _ -> False }
      newtypeDeriving = TH.DerivClause (Just TH.NewtypeStrategy)
          ([TH.ConT ''J.ToJSON, TH.ConT ''J.FromJSON] ++ if jsonKey then [TH.ConT ''J.ToJSONKey, TH.ConT ''J.FromJSONKey] else [])
  pure $ TH.NewtypeD [] tn [] Nothing (TH.NormalC ctor [(noBang, rhs)]) [stockDeriving, newtypeDeriving]
  --pure $ TH.TySynD tn [] rhs

(.=?) :: (J.KeyValue kv, J.ToJSON v) => J.Key -> Maybe v -> [kv]
k .=? v = case v of
  Just v' -> [k J..= v']
  Nothing -> mempty

-- | Generate a type declaration corresponding to a top-level named struct.
genStruct :: Structure -> CodeGenM [TH.Dec]
genStruct s@Structure{name, documentation, since, proposed} = do
  let structName = name

  st <- asks symbolTable
  tn <- case Map.lookup name st of
    Just n  -> pure n
    Nothing -> fail $ "no name for " ++ show name
  addDocumentation tn documentation since proposed

  ctor <- lift $ makeToplevelName name

  props <- getStructProperties s
  args <- for props $ \Property{name, type_, optional, documentation, since, proposed} -> do
      pty <- convertType type_
      let mty = case optional of
            Just True -> TH.ConT ''Maybe `TH.AppT` pty
            _         -> pty
      n <- makeFieldName name
      addDocumentation n documentation since proposed
      pure (name, (n, noBang, mty))

  let datad = TH.DataD [] tn [] Nothing [TH.RecC ctor (fmap snd args)] [stockDeriving]

  toJsonD <- do
    (unzip -> (args, pairEs)) <- for props $ \Property{name, optional} -> do
      n <- lift $ TH.newName "arg"
      pairE <- case optional of
        Just True -> lift [| $(TH.litE $ TH.stringL $ T.unpack name) .=? $(TH.varE n) |]
        _ -> lift [| [$(TH.litE $ TH.stringL $ T.unpack name) J..= $(TH.varE n)] |]
      pure (n, pairE)
    body <- lift [| J.object $ concat $ $(TH.listE (fmap pure pairEs)) |]
    let toJsonF = TH.FunD 'J.toJSON [TH.Clause [TH.ConP ctor (fmap TH.VarP args)] (TH.NormalB body) []]
    pure $ TH.InstanceD Nothing [] (TH.ConT ''J.ToJSON `TH.AppT` TH.ConT tn) [toJsonF]

  fromJsonD <- do
    vn <- lift $ TH.newName "v"
    exprs <- for props $ \Property{name, optional} ->
      case optional of
        Just True -> lift [| $(TH.varE vn) J..:! $(TH.litE $ TH.stringL $ T.unpack name)|]
        _         -> lift [| $(TH.varE vn) J..: $(TH.litE $ TH.stringL $ T.unpack name)|]
    let lamBody = mkIterApplicativeApp (TH.ConE ctor) exprs
    body <- lift [| J.withObject $(TH.litE $ TH.stringL $ T.unpack structName) $ \ $(TH.varP vn) -> $lamBody |]
    let fromJsonF = TH.FunD 'J.parseJSON [TH.Clause [] (TH.NormalB body) []]
    pure $ TH.InstanceD Nothing [] (TH.ConT ''J.FromJSON `TH.AppT` TH.ConT tn) [fromJsonF]

  pure [datad, toJsonD, fromJsonD]

-- | Get the list of properties of a struct, including inherited ones.
getStructProperties :: Structure -> CodeGenM [Property]
getStructProperties s@Structure{name, properties, extends, mixins} = do
  st <- asks structTable
  let
    extends' = fromMaybe [] extends
    mixins' = fromMaybe [] mixins
    supertypes = extends' ++ mixins'
  superProps <- for supertypes $ \case
    ReferenceType t | Just e <- Map.lookup t st -> getStructProperties e
    t -> fail $ "supertype of structure " ++ show name ++ " was not a reference to a structure: " ++ show t
  let allSuperProps = concat superProps
      -- If a property is redefined in the current type, then it overrides the inherited one
      localNames = foldMap (\Property{name} -> Set.singleton name) properties
      filteredSuperProps = filter (\Property{name} -> name `Set.notMember` localNames) allSuperProps
  pure (filteredSuperProps ++ properties)

-- | Generate a type corresponding to an anonymous struct.
genAnonymousStruct :: [Property] -> CodeGenM TH.Type
genAnonymousStruct properties = do
  row <- for properties $ \Property{name, type_, optional} -> do
    pty <- convertType type_
    let mty = case optional of
          Just True -> TH.ConT ''Maybe `TH.AppT` pty
          _         -> pty
    pure $ TH.ConT ''(.==) `TH.AppT` TH.LitT (TH.StrTyLit (T.unpack name)) `TH.AppT` mty
  let tyList = foldr (\ty l -> TH.ConT ''(.+) `TH.AppT` ty `TH.AppT` l) (TH.ConT ''Empty) row
  pure $ TH.AppT (TH.ConT ''Rec) tyList

--------------

data RequestData = RequestData
  { methCon                :: TH.Con
  , singCon                :: TH.Con
  , paramsEq               :: TH.TySynEqn
  , resultEq               :: TH.TySynEqn
  , errorDataEq            :: TH.TySynEqn
  , registrationOptionsEq  :: TH.TySynEqn
  , toStringClause         :: TH.Clause
  , fromStringClause       :: TH.Clause
  , messageDirectionClause :: TH.Clause
  , messageKindClause      :: TH.Clause
  }

data NotificationData = NotificationData
  { methCon                :: TH.Con
  , singCon                :: TH.Con
  , paramsEq               :: TH.TySynEqn
  , registrationOptionsEq  :: TH.TySynEqn
  , toStringClause         :: TH.Clause
  , fromStringClause       :: TH.Clause
  , messageDirectionClause :: TH.Clause
  , messageKindClause      :: TH.Clause
  }

data CustomData = CustomData
  { methCon                :: TH.Con
  , singCon                :: TH.Con
  , paramsEq               :: TH.TySynEqn
  , resultEq               :: TH.TySynEqn
  , errorDataEq            :: TH.TySynEqn
  , registrationOptionsEq  :: TH.TySynEqn
  , toStringClause         :: TH.Clause
  , fromStringClause       :: TH.Clause
  , messageDirectionClause :: TH.Clause
  , messageKindClause      :: TH.Clause
  }

-- TODO: partial result params
genMethods :: [Request] -> [Notification] -> CodeGenM [TH.Dec]
genMethods reqs nots = do
  mtyN <- lift $ TH.newName "Method"
  -- These are the type variables for the main type declaration, we will need them for
  -- some of the constructor declarations
  fN <- lift $ TH.newName "f"
  tN <- lift $ TH.newName "t"
  lift $ addHaddock mtyN "A type representing a LSP method (or class of methods), intended to be used mostly at the type level."

  styN <- lift $ TH.newName "SMethod"
  lift $ addHaddock styN "A singleton type for 'Method'."

  sstyN <- lift $ TH.newName "SomeMethod"
  lift $ addHaddock sstyN "A method which isn't statically known."
  -- We will want to refer to the constructor in some places
  smcn <- lift $ TH.newName "SomeMethod"

  mpN <- lift $ TH.newName "MessageParams"
  lift $ addHaddock mpN "Maps a LSP method to its parameter type."

  mrN <- lift $ TH.newName "MessageResult"
  lift $ addHaddock mrN "Maps a LSP method to its result type."

  edN <- lift $ TH.newName "ErrorData"
  lift $ addHaddock edN "Maps a LSP method to its error data type."

  roN <- lift $ TH.newName "RegistrationOptions"
  lift $ addHaddock roN "Maps a LSP method to its registration options type."

  toStringN <- lift $ TH.newName "someMethodToMethodString"
  lift $ addHaddock toStringN "Turn a 'SomeMethod' into its LSP method string."
  fromStringN <- lift $ TH.newName "methodStringToSomeMethod"
  lift $ addHaddock fromStringN "Turn a LSP method string into a 'SomeMethod'."

  mdN <- lift $ TH.newName "messageDirection"
  lift $ addHaddock mdN "Get a singleton witness for the message direction of a method."
  mtN <- lift $ TH.newName "messageKind"
  lift $ addHaddock mtN "Get a singleton witness for the message kind of a method."

  let methodName context fullName =
        let pieces = T.splitOn "/" fullName
        -- TODO: don't like this
        in makeConstrName (Just context) $ foldMap capitalize pieces
  let messagePartType t = case t of
        Just ty -> convertType ty
        -- See Note [Absent parameters/results/errors]
        Nothing -> lift [t| Maybe Void |]

  -- Construct the various pieces we'll need for the declarations in one go
  reqData <- for reqs $ \Request{method, params, result, errorData, registrationOptions, messageDirection} -> do
    -- <constructor name> :: Method <direction> <method type>
    mcn <- methodName "Method" method
    let direction = case messageDirection of
          MM.ClientToServer -> TH.PromotedT 'ClientToServer
          MM.ServerToClient -> TH.PromotedT 'ServerToClient
          MM.Both           -> TH.VarT fN
    let methCon = TH.GadtC [mcn] [] (mkIterAppT (TH.ConT mtyN) [direction, TH.PromotedT 'C.Request])

    scn <- methodName "SMethod" method
    let singCon = TH.GadtC [scn] [] (mkIterAppT (TH.ConT styN) [TH.PromotedT mcn])

    -- MessageParams <constructor name> = <param type>
    paramTy <- messagePartType params
    let paramsEq = TH.TySynEqn Nothing (TH.ConT mpN `TH.AppT` TH.ConT mcn) paramTy
    -- MessageResult <constructor name> = <result type>
    resultTy <- messagePartType (Just result)
    let resultEq = TH.TySynEqn Nothing (TH.ConT mrN `TH.AppT` TH.ConT mcn) resultTy
    errDatTy <- messagePartType errorData
    let errorDataEq = TH.TySynEqn Nothing (TH.ConT edN `TH.AppT` TH.ConT mcn) errDatTy
    regOptsTy <- messagePartType registrationOptions
    let registrationOptionsEq = TH.TySynEqn Nothing (TH.ConT roN `TH.AppT` TH.ConT mcn) regOptsTy

    r <- lift $ TH.newName "r"
    let
      mnLit = TH.StringL $ T.unpack method
      toStringClause = TH.Clause [TH.ConP smcn [TH.ConP scn []]] (TH.NormalB $ TH.LitE mnLit) []
      fromStringClause = TH.Clause [TH.LitP mnLit] (TH.NormalB $ TH.ConE smcn `TH.AppE` TH.ConE scn) []
      messageDirectionClause =
        let d = case messageDirection of
              MM.ClientToServer -> TH.ConE 'SClientToServer
              MM.ServerToClient -> TH.ConE 'SServerToClient
              MM.Both           -> TH.ConE 'SBothDirections
        in TH.Clause [TH.ConP scn []] (TH.NormalB d) []
      messageKindClause = TH.Clause [TH.ConP scn []] (TH.NormalB $ TH.ConE 'SRequest) []

    pure $ RequestData {..}

  notData <- for nots $ \Notification{method, params, registrationOptions, messageDirection} -> do
    mcn <- methodName "Method" method
    let direction = case messageDirection of
          MM.ClientToServer -> TH.PromotedT 'ClientToServer
          MM.ServerToClient -> TH.PromotedT 'ServerToClient
          MM.Both           -> TH.VarT fN
    let methCon = TH.GadtC [mcn] [] (mkIterAppT (TH.ConT mtyN) [direction, TH.PromotedT 'C.Notification])

    scn <- methodName "SMethod" method
    let singCon = TH.GadtC [scn] [] (mkIterAppT (TH.ConT styN) [TH.PromotedT mcn])

    -- MessageParams <constructor name> = <param type>
    paramTy <- messagePartType params
    let paramsEq = TH.TySynEqn Nothing (TH.ConT mpN `TH.AppT` TH.ConT mcn) paramTy
    regOptsTy <- messagePartType registrationOptions
    let registrationOptionsEq = TH.TySynEqn Nothing (TH.ConT roN `TH.AppT` TH.ConT mcn) regOptsTy

    r <- lift $ TH.newName "r"
    let
      mnLit = TH.StringL $ T.unpack method
      toStringClause = TH.Clause [TH.ConP smcn [TH.ConP scn []]] (TH.NormalB $ TH.LitE mnLit) []
      fromStringClause = TH.Clause [TH.LitP mnLit] (TH.NormalB $ TH.ConE smcn `TH.AppE` TH.ConE scn) []
      messageDirectionClause =
        let d = case messageDirection of
              MM.ClientToServer -> TH.ConE 'SClientToServer
              MM.ServerToClient -> TH.ConE 'SServerToClient
              MM.Both           -> TH.ConE 'SBothDirections
        in TH.Clause [TH.ConP scn []] (TH.NormalB d) []
      messageKindClause = TH.Clause [TH.ConP scn []] (TH.NormalB $ TH.ConE 'SNotification) []

    pure $ NotificationData {..}

  -- Add the custom method case, which isn't in the metamodel
  customDat <- do
    mcn <- methodName "Method" "CustomMethod"
    -- Method_CustomMethod :: Symbol -> Method f t
    let methCon = TH.GadtC [mcn] [(noBang, TH.ConT ''TL.Symbol)] (mkIterAppT (TH.ConT mtyN) [TH.VarT fN, TH.VarT tN])
    -- SMethod_CustomMethod :: KnownSymbol s => SMethod Method_CustomMethod
    scn <- methodName "SMethod" "CustomMethod"
    syn <- lift $ TH.newName "s"
    let singCon =
          TH.ForallC [TH.PlainTV syn TH.SpecifiedSpec] [TH.ConT ''TL.KnownSymbol `TH.AppT` TH.VarT syn] $
          TH.GadtC [scn] [(noBang, TH.ConT ''Proxy `TH.AppT` TH.VarT syn)] (TH.ConT styN `TH.AppT` (TH.PromotedT mcn `TH.AppT` TH.VarT syn))
    -- MessageParams Method_CustomMethod = Value
    let paramsEq = TH.TySynEqn Nothing (TH.ConT mpN `TH.AppT` (TH.ConT mcn `TH.AppT` TH.VarT syn)) (TH.ConT ''J.Value)
    -- MessageResult Method_CustomMethod = Value
    let resultEq = TH.TySynEqn Nothing (TH.ConT mrN `TH.AppT` (TH.ConT mcn `TH.AppT` TH.VarT syn)) (TH.ConT ''J.Value)
    -- Can shove whatever you want in the error data for custom methods?
    -- ErrorData Method_CustomMethod = Value
    let errorDataEq = TH.TySynEqn Nothing (TH.ConT edN `TH.AppT` (TH.ConT mcn `TH.AppT` TH.VarT syn)) (TH.ConT ''J.Value)
    -- Can't register custom methods
    -- RegistrationOptions Method_CustomMethod = Void
    let registrationOptionsEq = TH.TySynEqn Nothing (TH.ConT roN `TH.AppT` (TH.ConT mcn `TH.AppT` TH.VarT syn)) (TH.ConT ''Void)

    v <- lift $ TH.newName "v"
    fromStringBody <- lift [|
       case TL.someSymbolVal $(TH.varE v) of
         TL.SomeSymbol p -> SomeMethod (SMethod_CustomMethod p)
       |]
    let
      toStringClause = TH.Clause [TH.ConP smcn [TH.ConP scn [TH.VarP v]]] (TH.NormalB $ TH.VarE 'TL.symbolVal `TH.AppE` TH.VarE v) []
      fromStringClause = TH.Clause [TH.VarP v] (TH.NormalB fromStringBody) []
      messageDirectionClause = TH.Clause [TH.ConP scn [TH.WildP]] (TH.NormalB $ TH.ConE 'SBothDirections) []
      messageKindClause = TH.Clause [TH.ConP scn [TH.WildP]] (TH.NormalB $ TH.ConE 'SBothTypes) []

    pure $ CustomData {..}

  let mkMethodTv = do
        fN <- lift $ TH.newName "f"
        tN <- lift $ TH.newName "t"
        mN <- lift $ TH.newName "m"
        pure $ TH.KindedTV mN () (TH.ConT mtyN `TH.AppT` TH.VarT fN `TH.AppT` TH.VarT tN)

  dataD <- do
    let tyArgs = [TH.KindedTV fN () (TH.ConT ''MessageDirection), TH.KindedTV tN () (TH.ConT ''C.MessageKind)]
        ctors = fmap (\RequestData{..} -> methCon) reqData ++ fmap (\NotificationData{..} -> methCon) notData ++ [(\CustomData{..} -> methCon) customDat]
    -- This only really exists on the type level so we don't really want instances anyway
    pure $ TH.DataD [] mtyN tyArgs Nothing ctors []
  singD <- do
    mtv <- mkMethodTv
    let ctors = fmap (\RequestData{..} -> singCon) reqData ++ fmap (\NotificationData{..} -> singCon) notData ++ [(\CustomData{..} -> singCon) customDat]
    sig <- lift $ TH.kiSigD styN [t| forall f t . $(TH.conT mtyN) f t -> Kind.Type |]
    -- Can't derive instances, it's a GADT, will do them later
    let decl = TH.DataD [] styN [mtv] Nothing ctors []
    pure [sig, decl]
  ssmD <- do
    mn <- lift $ TH.newName "m"
    let ctor = TH.ForallC [TH.PlainTV mn TH.SpecifiedSpec] [] $ TH.GadtC [smcn] [(noBang, TH.ConT styN `TH.AppT` TH.VarT mn)] (TH.ConT sstyN)
    -- Can't derive instances because we're not doing the instances for SMethod here either
    pure $ TH.DataD [] sstyN [] Nothing [ctor] []
  mpD <- do
    mtv <- mkMethodTv
    sig <- lift $ TH.kiSigD mpN [t| forall f t . $(TH.conT mtyN) f t -> Kind.Type |]
    let eqns = fmap (\RequestData{..} -> paramsEq) reqData ++ fmap (\NotificationData{..} -> paramsEq) notData ++ [(\CustomData{..} -> paramsEq) customDat]
        hd = TH.TypeFamilyHead mpN [mtv] (TH.KindSig (TH.ConT ''Kind.Type)) Nothing
        decl = TH.ClosedTypeFamilyD hd eqns
    pure [sig, decl]
  mrD <- do
    mtv <- mkMethodTv
    sig <- lift $ TH.kiSigD mrN [t| forall f t . $(TH.conT mtyN) f t -> Kind.Type |]
    let eqns = fmap (\RequestData{..} -> resultEq) reqData ++ [(\CustomData{..} -> resultEq) customDat]
        hd = TH.TypeFamilyHead mrN [mtv] (TH.KindSig (TH.ConT ''Kind.Type)) Nothing
        decl = TH.ClosedTypeFamilyD hd eqns
    pure [sig, decl]
  edD <- do
    mtv <- mkMethodTv
    sig <- lift $ TH.kiSigD edN [t| forall f t . $(TH.conT mtyN) f t -> Kind.Type |]
    let eqns = fmap (\RequestData{..} -> errorDataEq) reqData ++ [(\CustomData{..} -> errorDataEq) customDat]
        hd = TH.TypeFamilyHead edN [mtv] (TH.KindSig (TH.ConT ''Kind.Type)) Nothing
        decl = TH.ClosedTypeFamilyD hd eqns
    pure [sig, decl]
  roD <- do
    mtv <- mkMethodTv
    sig <- lift $ TH.kiSigD roN [t| forall f t . $(TH.conT mtyN) f t -> Kind.Type |]
    let eqns = fmap (\RequestData{..} -> registrationOptionsEq) reqData ++ fmap (\NotificationData{..} -> registrationOptionsEq) notData ++ [(\CustomData{..} -> registrationOptionsEq) customDat]
        hd = TH.TypeFamilyHead roN [mtv] (TH.KindSig (TH.ConT ''Kind.Type)) Nothing
        decl = TH.ClosedTypeFamilyD hd eqns
    pure [sig, decl]

  -- methodToString :: SomeMethod -> Text
  toStringD <- do
    let clauses = fmap (\RequestData{..} -> toStringClause) reqData ++ fmap (\NotificationData{..} -> toStringClause) notData ++ [(\CustomData{..} -> toStringClause) customDat]
        sd = TH.SigD toStringN (TH.ArrowT `TH.AppT` TH.ConT sstyN `TH.AppT` TH.ConT ''String)
        fd = TH.FunD toStringN clauses
    pure [ sd, fd ]
  -- stringToMethod :: Text -> SomeMethod
  fromStringD <- do
    let clauses = fmap (\RequestData{..} -> fromStringClause) reqData ++ fmap (\NotificationData{..} -> fromStringClause) notData ++ [(\CustomData{..} -> fromStringClause) customDat]
        sd = TH.SigD fromStringN (TH.ArrowT `TH.AppT` TH.ConT ''String `TH.AppT` TH.ConT sstyN)
        fd = TH.FunD fromStringN clauses
    pure [ sd, fd ]

  messageSingD <- do
    let fn = TH.mkName "f"
    let tn = TH.mkName "t"
    let mn = TH.mkName "m"
    let mty = TH.ConT mtyN `TH.AppT` TH.VarT fn `TH.AppT` TH.VarT tn
        tyVars = [TH.PlainTV fn TH.SpecifiedSpec, TH.PlainTV tn TH.SpecifiedSpec, TH.KindedTV mn TH.SpecifiedSpec mty]
        inTy = TH.ConT styN `TH.AppT` TH.VarT mn

    -- messageDirection
    let clauses = fmap (\RequestData{..} -> messageDirectionClause) reqData ++ fmap (\NotificationData{..} -> messageDirectionClause) notData ++ [(\CustomData{..} -> messageDirectionClause) customDat]
        outTy = TH.ConT ''SMessageDirection `TH.AppT` TH.VarT fn
        funTy = TH.ArrowT `TH.AppT` inTy `TH.AppT` outTy
        sd1 = TH.SigD mdN (TH.ForallT tyVars [] funTy)
        fd1 = TH.FunD mdN clauses
    -- messageKind
    let clauses = fmap (\RequestData{..} -> messageKindClause) reqData ++ fmap (\NotificationData{..} -> messageKindClause) notData ++ [(\CustomData{..} -> messageKindClause) customDat]
        outTy = TH.ConT ''SMessageKind `TH.AppT` TH.VarT tn
        funTy = TH.ArrowT `TH.AppT` inTy `TH.AppT` outTy
        sd2 = TH.SigD mtN (TH.ForallT tyVars [] funTy)
        fd2 = TH.FunD mtN clauses
    pure [ sd1, fd1, sd2, fd2 ]

  pure $ [dataD] ++ singD ++ [ssmD] ++ mpD ++ mrD ++ edD ++ roD ++ toStringD ++ fromStringD ++ messageSingD

--------------

addDocumentation :: TH.Name -> Maybe Text -> Maybe Text -> Maybe Bool -> CodeGenM ()
addDocumentation nm doc since proposed =
  let docLines = catMaybes [doc, since, fmap (T.pack . show) proposed ]
  in
    if null docLines
    then pure ()
    else lift $ addHaddock nm $ T.unlines docLines

-----------------

capitalize :: T.Text -> T.Text
capitalize s = T.toUpper (T.singleton (T.head s)) `T.append` T.tail s

uncapitalize :: T.Text -> T.Text
uncapitalize s = T.toLower (T.singleton (T.head s)) `T.append` T.tail s

mkIterAppT :: TH.Type -> [TH.Type] -> TH.Type
mkIterAppT = foldl' TH.AppT

mkNestedTupleT :: [TH.Type] -> TH.Type
mkNestedTupleT es =
  let l = length es
  in if l < 62
  then mkIterAppT (TH.TupleT l) es
  else let (es1, es2) = splitAt (l `div` 2) es
       in mkIterAppT (TH.TupleT 2) [mkNestedTupleT es1, mkNestedTupleT es2]

-- | Generate code of the form 'hd <$> a1 <*> ... <*> an'
mkIterApplicativeApp :: TH.Exp -> [TH.Exp] -> TH.Q TH.Exp
mkIterApplicativeApp hd = go
    where
      go [] = [| pure $(pure hd) |]
      go (a:rest) = do
        acc <- [| $(pure hd) <$> $(pure a)|]
        go' acc rest
      go' acc [] = pure acc
      go' acc (a:rest) = do
        acc' <- [| $(pure acc) <*> $(pure a)|]
        go' acc' rest

noBang :: TH.Bang
noBang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness

stockDeriving :: TH.DerivClause
stockDeriving = TH.DerivClause (Just TH.StockStrategy)
  [ TH.ConT ''Show
  , TH.ConT ''Eq
  , TH.ConT ''Ord
  -- This makes things very slow
  --, TH.ConT ''Generic
  ]

{- Note [Anonymous records]
We need anonymous records in a few places. We could lift each of these to the top
level and declare a new Haskell record type for them, but this requires us to make
lots of arbitrary choices (e.g. what do we call all these new types?) and takes us
further from representing the metamodel accurately. So we instead use an actual
anonymous records library, in this case `row-types`.
-}

{- Note [Avoiding name clashes]
It is difficult to avoid name clashes, especially since we don't control the input
source. And there are plenty of name clashes in the metamodel.

One approach would be to generate lots of modules and use Haskell's module system
to disambiguate. But this is not easy with TH: we'd have to write each module ourselves
and split up the logic, whereas it actually makes much more sense to do the generation
all together.

So that suggests we just need to pick non-clashing names. The crude heuristic
we have adopted is to prefix many values with the name of the type with which they
are associated, followed by an underscore. So the constructors of `X` will be
`X_A`, `X_B` etc.

We don't do this for fields, instead we rely on `DuplicateRecordFields` and
use classy lenses.
-}

{- See Note [Translating metamodel types]

= Or types

Or types are translated directly into anonymous unions using '(|?)'.

= And types

And types are difficult to handle in general (it's not even clear what that means). We assume
that they contain only references to structures, and translate them as anonymous records
with the union of the fields of the components of the and type.

= Null

We would like a type that reliably serializes to/from null, since null alternatives
are called out explicitly in the LSP spec. In the end, we just defined our own: 'Null'.

= Enumerations

Enumerations are compiled as simple sum types.

Enums that allow custom values get a special extra constructor for that.

= Type aliases

Type aliases are compiled to newtype definitions.

The alternative would be to compile them to type aliases. It's not at all clear which
one is better, but this way is closer to how we did things before and in some cases
makes things easier (e.g. when there is a type alias for an anoymous record you get
slightly better errors before you go under the newtype).

= Structures

Top level strutures are compiled into record datatypes.

Properties for structures are included in the following order:
- Properties from types in 'extends' (including all their inherited properties).
- Properties from types in 'mixins' (including all their inherited properties).
- Properties defined in the struct itself.

We insist that extended and mixed in types are references to top-level structures (it's
unclear that anything else makes sense).

Field names for structure properties are not disambiguated: we rely on `DuplicateRecordFields`.
We generate lenses for conveniently accessing all the duplicate fields, hence
the fields themselves are prefixed with an underscore so they don't clash with the lenses.

== Optional fields

Optional fields are translated as 'Maybe' types. We can configure `aeson` to do the right thing
for datatypes, and for anonymous records we have our own instances in 'Data.Row.Aeson'.

== Structure literals

Structure literals are translated directly as anonymous records.

== String/integer literals

String and integer literal types are weird. They're inhabited by only that specific
string or integer. They're often used for "kind" fields i.e. to encode sum types.
We do try to represent this faithfully, so we have types 'AString' and 'AnInteger'
which behave like this.

-}

{- Note [Generating code for methods]
The code generation for methods is in many ways the most complicated part,
because there are some type-level parts. We follow the same basic approach as the
old way:
- A 'Method' type that represents a method, with type parameters for direction and
type (notification/request).
- A 'SMethod' singleton GADT indexed by a 'Method' that can be passed around at runtime.
- A variety of type families for mapping 'Method's to their parameter types, result types, etc.

We also generate a few functions. The ultimate goal would be to avoid any non-generated
code having to do a full pattern match on 'Method', since it's gigantic and that's not
very maintainable. We don't quite achieve that yet.
-}

{- Note [Absent parameters/results/errors]
Many methods don't *have* parameters/results/errors. What are we supposed to do there?
We can't say the type is 'Null', because the client will send us messages where the
value is absent, not just null. We really need a way to say the value is *absent*.

We have a cunning trick for this: use 'Maybe Void'. That can only ever be 'Nothing',
and sine we're configuring aeson to omit 'Nothing' fields in objects, that's exactly
what we want.

See also https://github.com/haskell/aeson/issues/646 for some relevant discussion.
-}

{- Note [Generating lenses]
We would like to use the TH from 'lens' to generate lenses. However, this is tricky because
a) It uses 'reify'
b) It creates typeclass instances

Fact a) means it can't be run in the same splice as the one in which we generate the
types. The normal workaround is to run it with 'addModFinalizer'... but code which
runs there can't generate typecalss instances!

So we're a bit stuck. The solution I came up with is to generate a binding that lists
all the names of the types we want to generate lenses for (as 'String's, not 'Name's,
see https://gitlab.haskell.org/ghc/ghc/-/issues/21759), and then run the lens-generation
TH in another module. Which is clunky, but works.
-}
