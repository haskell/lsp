{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{- | The main module for generating code from the metamodel

See Note [Code generation approach] for why we do it this way.
-}
module CodeGen where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Foldable
import Data.Function
import Data.List (intersperse, sort)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable
import Language.LSP.MetaModel as MM
import Prettyprinter
import System.Directory
import System.FilePath
import Text.RE.Replace qualified as RE
import Text.RE.TDFA.Text qualified as RE

-- | A mapping from names in the metamodel to their names in the generated Haskell.
type SymbolTable = Map.Map T.Text T.Text

{- | A mapping from names in the metamodel to their structure definition, used for chasing
 supertypes.
-}
type StructTable = Map.Map T.Text Structure

data CodeGenEnv = CodeGenEnv
  { symbolTable :: SymbolTable
  , structTable :: StructTable
  , modulePrefix :: T.Text
  , outputDir :: FilePath
  }

-- | Monad for running overall code generation in, has access to the environment and settings.
type CodeGenM = ReaderT CodeGenEnv IO

-- | Monad for running module generation in, the same as 'CodeGenM' with the ability to record imports.
type ModuleGenM = WriterT (Set.Set T.Text) CodeGenM

typesModSegment :: T.Text
typesModSegment = "Types"

pragma :: T.Text -> Doc ann -> Doc ann
pragma kind doc = "{-#" <+> pretty kind <+> doc <+> "#-}"

-- comment out Generic for a faster build...
toStockDerive :: [T.Text]
toStockDerive = ["Show", "Eq", "Ord", "Generic"]

toAnyclassDerive :: [T.Text]
toAnyclassDerive = ["NFData", "Hashable"]

indentSize :: Int
indentSize = 2

optDeprecated :: T.Text -> Maybe T.Text -> [Doc ann]
optDeprecated name mreason = case mreason of
  Just reason -> ["{-# DEPRECATED" <+> pretty name <+> dquotes (pretty reason) <+> "#-}"]
  Nothing -> []

-- TODO: since and proposed are a mess, figure out whether there's a useful way to include them
mkDocumentation :: Maybe T.Text -> Maybe T.Text -> Maybe Bool -> ModuleGenM (Maybe T.Text)
mkDocumentation (Just doc) _since proposed = Just <$> fixupDocumentation doc
mkDocumentation Nothing _since proposed = pure Nothing

fixupDocumentation :: T.Text -> ModuleGenM T.Text
fixupDocumentation t = do
  -- TODO: use the symbol table to map these to the actual entity names
  let
    fixupJSDocLinks t = RE.replaceAll "`$1`" (t RE.*=~ [RE.re|{@link[[:space:]]+$([[:word:].]+).*}|])
    fixupMarkdownLinks t = RE.replaceAll "`$1`" $ t RE.*=~ [RE.re|\[[[:word:].]+\][[:space:]]*\(#$([[:word:].]+)\)|]
  let t' = fixupJSDocLinks t
  let t'' = fixupMarkdownLinks t'
  pure t''

multilineHaddock :: Doc ann -> Doc ann
multilineHaddock doc = vsep ["{-|", doc, "-}"]

genModule :: forall ann. T.Text -> [T.Text] -> Maybe [T.Text] -> ModuleGenM (Doc ann) -> CodeGenM T.Text
genModule name pragmas mexports action = do
  (doc, imports) <- runWriterT action
  mp <- asks modulePrefix
  dir <- asks outputDir
  let
    -- these are both common in the generated code
    ghcOptions :: [T.Text] = ["-Wno-unused-imports", "-Wno-unused-matches", "-Wno-deprecations"]
    fullModName = mp <> "." <> name
    ignoreComments = ["{- ORMOLU_DISABLE -}", "{- HLINT ignore -}"]
    warning = "-- THIS IS A GENERATED FILE, DO NOT EDIT"
    pragmaSection = hardvcat (fmap (\p -> "{-#" <+> "LANGUAGE" <+> pretty p <+> "#-}") pragmas)
    optionsSection = hardvcat (fmap (\p -> "{-#" <+> "OPTIONS_GHC" <+> pretty p <+> "#-}") ghcOptions)
    header = case mexports of
      Just exports -> "module" <+> pretty fullModName <+> parens (cat $ punctuate "," (fmap pretty exports)) <+> "where"
      Nothing -> "module" <+> pretty fullModName <+> "where"
    -- TODO: replace with regex
    isSelfImport imp = (" " <> fullModName <> " ") `T.isInfixOf` imp || (" " <> fullModName) `T.isSuffixOf` imp
    importSection = hardvcat (fmap pretty $ filter (not . isSelfImport) $ toList imports)
    mod =
      hardvcat ignoreComments
        <> hardline
        <> warning
        <> hardline
        <> pragmaSection
        <> hardline
        <> optionsSection
        <> hardline
        <> header
        <> hardline
        <> hardline
        <> importSection
        <> hardline
        <> hardline
        <> doc
        <> hardline
    printed = T.pack $ show mod

    modSegments = T.unpack <$> T.splitOn "." fullModName
    modulePath = foldl (</>) dir modSegments <.> "hs"

  lift $ createDirectoryIfMissing True $ takeDirectory modulePath
  lift $ T.writeFile modulePath printed
  pure fullModName

data Qualification = Unqual | Qual | QualAs T.Text | As T.Text

ensureImport :: T.Text -> Qualification -> ModuleGenM T.Text
ensureImport mod Unqual = tell (Set.singleton $ "import " <> mod) >> pure mod
ensureImport mod Qual = tell (Set.singleton $ "import qualified " <> mod) >> pure mod
ensureImport mod (QualAs qual) = tell (Set.singleton $ "import qualified " <> mod <> " as " <> qual) >> pure qual
ensureImport mod (As qual) = tell (Set.singleton $ "import " <> mod <> " as " <> qual) >> pure qual

ensureLSPImport :: T.Text -> Qualification -> ModuleGenM T.Text
ensureLSPImport mod qual = do
  mp <- asks modulePrefix
  ensureImport (mp <> "." <> mod) qual

entityName :: T.Text -> T.Text -> ModuleGenM T.Text
entityName mod n = do
  qual <- ensureImport mod Qual
  pure $ qual <> "." <> n

lspEntityName :: T.Text -> T.Text -> ModuleGenM T.Text
lspEntityName mod n = do
  qual <- ensureLSPImport mod Qual
  pure $ qual <> "." <> n

genFromMetaModel :: T.Text -> FilePath -> MetaModel -> IO ()
genFromMetaModel prefix dir mm = do
  let (symbolTable, structTable) = buildTables mm
  flip runReaderT (CodeGenEnv symbolTable structTable prefix dir) $ do
    -- Don't even generate LSPAny, LSPObject, or LSPArry
    let filteredAliases = filter (\TypeAlias{name} -> name `notElem` ["LSPAny", "LSPObject", "LSPArray"]) (typeAliases mm)
    structModuleNames <- traverse genStruct (structures mm)
    aliasModuleNames <- traverse genAlias filteredAliases
    enumModuleNames <- traverse genEnum (enumerations mm)
    methodModuleName <- genMethods (requests mm) (notifications mm)
    -- not the methods, we export them separately!
    genAllModule $ sort $ concat [structModuleNames, aliasModuleNames, enumModuleNames]
    -- Have to use the string form of the generated Name
    -- since we might have mangled the original name
    let structNames = mapMaybe (\Structure{name} -> Map.lookup name symbolTable) (structures mm)
        aliasNames = mapMaybe (\TypeAlias{name} -> Map.lookup name symbolTable) filteredAliases
        enumNames = mapMaybe (\Enumeration{name} -> Map.lookup name symbolTable) (enumerations mm)
    genMetaModule structNames aliasNames enumNames
    pure ()
  pure ()

-- | Names we can't put in Haskell code.
reservedNames :: Set.Set T.Text
reservedNames = Set.fromList ["data", "type"]

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
makeToplevelName :: T.Text -> T.Text
makeToplevelName n = capitalize $ sanitizeName n

-- | Make a name for a constructor, optionally including a contextual name to qualify it with.
makeConstrName :: Maybe T.Text -> T.Text -> T.Text
makeConstrName context n =
  let
    cap = capitalize n
    disambiguated = case context of Just t -> t <> "_" <> cap; Nothing -> cap
   in
    sanitizeName disambiguated

-- | Make a name for a field.
makeFieldName :: T.Text -> T.Text
makeFieldName n = "_" <> sanitizeName n

buildTables :: MetaModel -> (SymbolTable, StructTable)
buildTables (MetaModel{structures, enumerations, typeAliases}) =
  let bothEntries = flip fmap structures $ \s@Structure{name} ->
        ((name, makeToplevelName name), (name, s))
      (entries, sentries) = unzip bothEntries

      entries' = flip fmap enumerations $ \Enumeration{name} -> (name, makeToplevelName name)

      entries'' = flip fmap typeAliases $ \TypeAlias{name} -> (name, makeToplevelName name)
      symbolTable = Map.fromList $ entries <> entries' <> entries''

      structTable = Map.fromList sentries
   in (symbolTable, structTable)

{- | Translate a type in the metamodel into the corresponding Haskell type.
 See Note [Translating metamodel types]
-}
convertType :: Type -> ModuleGenM (Doc ann)
convertType = \case
  BaseType n -> case n of
    URI -> pretty <$> entityName "Language.LSP.Protocol.Types.Uri" "Uri"
    DocumentUri -> pretty <$> entityName "Language.LSP.Protocol.Types.Uri" "Uri"
    Integer -> pretty <$> entityName "Language.LSP.Protocol.Types.Common" "Int32"
    UInteger -> pretty <$> entityName "Language.LSP.Protocol.Types.Common" "UInt"
    Decimal -> pure "Float"
    RegExp -> pretty <$> entityName "Data.Text" "Text"
    String -> pretty <$> entityName "Data.Text" "Text"
    Boolean -> pure "Bool"
    Null -> pretty <$> entityName "Language.LSP.Protocol.Types.Common" "Null"
  -- Special cases: these are in fact defined in the meta model, but
  -- we have way better types for them

  -- 'LSPAny' is a big union of anything in the metamodel, we just
  -- keep that as an aeson 'Value'
  ReferenceType "LSPAny" -> pretty <$> entityName "Data.Aeson" "Value"
  -- 'LSPObject' is an empty structure ... better to just say it's an aeson 'Object'!
  ReferenceType "LSPObject" -> pretty <$> entityName "Data.Aeson" "Object"
  -- 'LSPArray' is a list of 'LSPAny'... better to just say it's an aeson 'Array'!
  ReferenceType "LSPArray" -> pretty <$> entityName "Data.Aeson" "Array"
  ReferenceType n -> do
    st <- asks symbolTable
    case Map.lookup n st of
      Just thn -> pretty <$> lspEntityName (typesModSegment <> "." <> thn) thn
      Nothing -> fail $ "Reference to unknown type: " <> show n
  ArrayType e -> do
    innerType <- convertType e
    pure $ brackets innerType
  MapType k v -> do
    kt <- convertType k
    vt <- convertType v
    n <- pretty <$> entityName "Data.Map" "Map"
    pure $ parens $ n <+> kt <+> vt
  OrType es -> do
    est <- traverse convertType es
    n <- pretty <$> entityName "Language.LSP.Protocol.Types.Common" "|?"
    pure $ foldr1 (\ty o -> parens (ty <+> n <+> o)) est
  AndType es -> do
    st <- asks structTable
    props <- for es $ \case
      ReferenceType t | Just e <- Map.lookup t st -> getStructProperties e
      t -> fail $ "element of 'and' type was not a reference to a structure: " ++ show t
    genAnonymousStruct $ concat props
  StructureLiteralType (StructureLiteral{properties}) -> genAnonymousStruct properties
  TupleType es -> do
    est <- traverse convertType es
    pure $ tupled est
  StringLiteralType s -> do
    tycon <- entityName "Language.LSP.Protocol.Types.Singletons" "AString"
    let ty = pretty tycon <+> dquotes (pretty s)
    pure $ parens ty
  IntegerLiteralType n -> do
    tycon <- entityName "Language.LSP.Protocol.Types.Singletons" "AnInteger"
    let ty = pretty tycon <+> pretty n
    pure $ parens ty
  BooleanLiteralType _ -> fail "unsupported: boolean literal types"

genStruct :: Structure -> CodeGenM T.Text
genStruct s@Structure{name} = do
  st <- asks symbolTable
  hsName <- case Map.lookup name st of
    Just hsn -> pure hsn
    Nothing -> fail $ "Unknown type: " <> show name
  genModule (typesModSegment <> "." <> hsName) [] Nothing (printStruct hsName s)

printStruct :: T.Text -> Structure -> ModuleGenM (Doc ann)
printStruct tn s@Structure{name, documentation, since, proposed, deprecated} = do
  let structName = name

  props <- getStructProperties s
  args <- for props $ \Property{name, type_, optional, documentation, since, proposed, deprecated} -> do
    pty <- convertType type_
    let mty = case optional of
          Just True -> parens ("Maybe" <+> pty)
          _ -> pty
    let n = makeFieldName name
    propDoc <- multilineHaddock . pretty <$> mkDocumentation documentation since proposed
    pure $ hardvcat [propDoc, pretty n <+> "::" <+> mty]

  -- We do *not* deprecate fields. We can't really represent this properly: typically a deprecated field
  -- is optional, and the "correct" thing to do is to omit it. But in our representaiton that means passing
  -- Nothing as the value, which counts as "using" the field. So there is no way for users to avoid the
  -- deprecation warning, which is silly. To do this properly we'd need to do something clever with
  -- pattern synonyms maybe?
  let deprecations = optDeprecated tn deprecated

  ensureImport "GHC.Generics" Unqual
  ensureImport "Control.DeepSeq" Unqual
  ensureImport "Data.Hashable" Unqual
  ensureImport "Prettyprinter" Unqual
  ensureImport "Language.LSP.Protocol.Utils.Misc" Unqual
  let derivDoc =
        let stockDeriv = "deriving stock" <+> tupled (fmap pretty toStockDerive)
            anyclassDeriv = "deriving anyclass" <+> tupled (fmap pretty toAnyclassDerive)
            viaDeriv = "deriving" <+> "Pretty" <+> "via" <+> parens ("ViaJSON" <+> pretty tn)
         in indent indentSize $ hardvcat [stockDeriv, anyclassDeriv, viaDeriv]
  dataDoc <- multilineHaddock . pretty <$> mkDocumentation documentation since proposed
  let dataDecl = "data" <+> pretty tn <+> "=" <+> pretty tn <+> nest indentSize (encloseSep (line <> "{ ") (line <> "}") ", " args)
      datad = hardvcat (deprecations ++ [dataDoc, dataDecl, derivDoc])

  ensureImport "Data.Aeson" (QualAs "Aeson")
  ensureImport "Data.Row.Aeson" (QualAs "Aeson")
  ensureImport "Data.Row.Hashable" (QualAs "Hashable")
  optionalPairerName <- entityName "Language.LSP.Protocol.Types.Common" ".=?"
  optionalMatcherName <- entityName "Language.LSP.Protocol.Types.Common" ".:!?"
  let toJsonD =
        let (unzip -> (args, pairEs)) = flip fmap (zip props [0 ..]) $ \(Property{name, optional}, i) ->
              let n :: T.Text = "arg" <> (T.pack $ show i)
                  pairE = case optional of
                    Just True -> dquotes (pretty name) <+> pretty optionalPairerName <+> pretty n
                    _ -> brackets (dquotes (pretty name) <+> "Aeson..=" <+> pretty n)
               in (pretty n, pairE)
            body = "Aeson.object $ concat $ " <+> encloseSep "[" "]" "," pairEs
            toJsonDoc = "toJSON" <+> parens (pretty tn <+> hsep args) <+> "=" <+> nest indentSize body
            instanceDoc = "instance Aeson.ToJSON" <+> pretty tn <+> "where" <> nest indentSize (hardline <> toJsonDoc)
         in instanceDoc

  fromJsonD <- do
    let vn :: T.Text = "arg"
    let exprs = flip fmap props $ \Property{name, optional} ->
          case optional of
            -- Accept null in place of Nothing
            -- Note [Principle of robustness for parsing LSP types]
            Just True -> pretty vn <+> pretty optionalMatcherName <+> dquotes (pretty name)
            _ -> pretty vn <+> "Aeson..:" <+> dquotes (pretty name)
    let lamBody = mkIterApplicativeApp (pretty tn) exprs
    let body = "Aeson.withObject" <+> dquotes (pretty structName) <+> "$" <+> "\\" <> pretty vn <+> "->" <+> nest indentSize lamBody
    let fromJsonDoc = "parseJSON" <+> "=" <+> nest indentSize body
    let instanceDoc = "instance Aeson.FromJSON" <+> pretty tn <+> "where" <> nest indentSize (hardline <> fromJsonDoc)
    pure instanceDoc

  pure $
    datad
      <> hardline
      <> hardline
      <> toJsonD
      <> hardline
      <> hardline
      <> fromJsonD

-- | Get the list of properties of a struct, including inherited ones.
getStructProperties :: Structure -> ModuleGenM [Property]
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
genAnonymousStruct :: [Property] -> ModuleGenM (Doc ann)
genAnonymousStruct properties = do
  row <- for properties $ \Property{name, type_, optional} -> do
    pty <- convertType type_
    let mty = case optional of
          Just True -> parens ("Maybe" <+> pty)
          _ -> pty
    ensureImport "Data.Row" (QualAs "Row")
    pure $ dquotes (pretty name) <+> "Row..==" <+> mty
  let tyList = foldr (\ty l -> parens $ ty <+> "Row..+" <+> l) "Row.Empty" row
  pure $ parens $ "Row.Rec" <+> tyList

genEnum :: Enumeration -> CodeGenM T.Text
genEnum e@Enumeration{name} = do
  st <- asks symbolTable
  hsName <- case Map.lookup name st of
    Just hsn -> pure hsn
    Nothing -> fail $ "Unknown type: " <> show name
  genModule (typesModSegment <> "." <> hsName) [] Nothing (printEnum hsName e)

printEnum :: T.Text -> Enumeration -> ModuleGenM (Doc ann)
printEnum tn Enumeration{name, type_, values, supportsCustomValues, documentation, since, proposed, deprecated} = do
  st <- asks symbolTable

  let enumName = name
      enumNameString = T.unpack enumName
      -- This indicates whether or not the enum is "open" and supports custom values.
      -- We need to branch on this a lot!
      custom = fromMaybe False supportsCustomValues

  -- The (Haskell) type of the elements of this enum. Useful, so we can generate various
  -- code (e.g. for parsing JSON) generically but use this type to pin down what we want to do.
  ty <- case type_ of
    BaseType Integer -> pretty <$> entityName "Language.LSP.Protocol.Types.Common" "Int32"
    BaseType UInteger -> pretty <$> entityName "Language.LSP.Protocol.Types.Common" "UInt"
    BaseType String -> pretty <$> entityName "Data.Text" "Text"
    _ -> fail $ "enumeration of unexpected type " ++ show type_

  let isString = case type_ of
        BaseType String -> True
        _ -> False

  -- https://github.com/microsoft/vscode-languageserver-node/issues/1035
  let badEnumValues = ["jsonrpcReservedErrorRangeStart", "jsonrpcReservedErrorRangeEnd", "serverErrorStart", "serverErrorEnd"]
      values' = filter (\EnumerationEntry{name} -> name `notElem` badEnumValues) values
  -- The associations between constructor names and their literals
  assocs <- for values' $ \EnumerationEntry{name, value, documentation, since, proposed} -> do
    let cn = makeConstrName (Just enumName) name
        -- The literal for the actual enum value in this case
        lit = case value of
          T t -> pretty $ show $ T.unpack t
          I i -> pretty $ show i
    doc <- mkDocumentation documentation since proposed
    pure (cn, lit, doc)

  let normalCons = flip fmap assocs $ \(cn, _, doc) ->
        hardvcat [multilineHaddock $ pretty doc, pretty cn]
  let customCon =
        let cn = makeConstrName (Just enumName) "Custom"
         in if custom then Just (cn, pretty cn <+> ty) else Nothing
  let cons = normalCons ++ (fmap snd $ maybeToList customCon)

  ensureImport "Data.Aeson" (QualAs "Aeson")
  ensureImport "Data.Row.Aeson" (QualAs "Aeson")
  ensureImport "Data.Row.Hashable" (QualAs "Hashable")

  lspEnumN <- pretty <$> entityName "Language.LSP.Protocol.Types.LspEnum" "LspEnum"
  let knownValuesN = "knownValues"
  let toBaseTypeN = "toEnumBaseType"
  let fromBaseTypeN = "fromEnumBaseType"
  lspOpenEnumN <- pretty <$> entityName "Language.LSP.Protocol.Types.LspEnum" "LspOpenEnum"
  let fromOpenBaseTypeN = "fromOpenEnumBaseType"
  asLspEnumN <- pretty <$> entityName "Language.LSP.Protocol.Types.LspEnum" "AsLspEnum"
  isStringN <- pretty <$> entityName "Data.String" "IsString"

  let deprecations = optDeprecated tn deprecated ++ (flip concatMap values' $ \EnumerationEntry{name, deprecated} -> optDeprecated (makeConstrName (Just enumName) name) deprecated)

  ensureImport "GHC.Generics" Unqual
  ensureImport "Control.DeepSeq" Unqual
  ensureImport "Data.Hashable" Unqual
  ensureImport "Prettyprinter" Unqual
  ensureImport "Language.LSP.Protocol.Utils.Misc" Unqual
  dataDoc <- multilineHaddock . pretty <$> mkDocumentation documentation since proposed
  let derivDoc =
        let
          toDeriveViaLspEnum = ["Aeson.ToJSON", "Aeson.FromJSON"] ++ if custom && isString then [isStringN] else []
          stockDeriv = "deriving stock" <+> tupled (fmap pretty toStockDerive)
          anyclassDeriv = "deriving anyclass" <+> tupled (fmap pretty toAnyclassDerive)
          viaDeriv1 = "deriving" <+> tupled toDeriveViaLspEnum <+> "via" <+> parens (asLspEnumN <+> pretty tn)
          viaDeriv2 = "deriving" <+> "Pretty" <+> "via" <+> parens ("ViaJSON" <+> pretty tn)
         in
          indent indentSize $ hardvcat [stockDeriv, anyclassDeriv, viaDeriv1, viaDeriv2]
  let dataDecl = "data" <+> pretty tn <+> "=" <+> nest indentSize (encloseSep (line <> "  ") mempty "| " cons)
      dataD = hardvcat (deprecations ++ [dataDoc, dataDecl, derivDoc])

  setFromListN <- pretty <$> entityName "Data.Set" "fromList"
  let knownValuesD =
        let valuesList = nest indentSize $ encloseSep "[" "]" "," $ flip fmap assocs $ \(n, _, _) -> pretty n
         in knownValuesN <+> "=" <+> setFromListN <+> valuesList

  let toBaseTypeD =
        -- xToValue X1 = <X1 value>
        let normalClauses = flip fmap assocs $ \(n, v, _) -> toBaseTypeN <+> pretty n <+> "=" <+> v
            -- xToValue (CustomX c) = c
            customClause = case customCon of
              Just (cn, _) ->
                let vn :: T.Text = "arg"
                 in Just $ toBaseTypeN <+> parens (pretty cn <+> pretty vn) <+> "=" <+> pretty vn
              Nothing -> Nothing
            clauses = normalClauses ++ maybeToList customClause
         in hardvcat clauses

  let fromBaseTypeD =
        let fn = if custom then fromOpenBaseTypeN else fromBaseTypeN
            -- valueToX <X1 value> = X
            -- or
            -- valueToX <X1 value> = Just X
            normalClauses = flip fmap assocs $ \(n, v, _) -> fn <+> v <+> "=" <+> if custom then pretty n else "pure" <+> pretty n
            -- valueToX c = CustomX c
            -- or
            -- valueToX _ = Nothing
            fallThroughClause = case customCon of
              Just (cn, _) ->
                let vn :: T.Text = "arg"
                 in fn <+> pretty vn <+> "=" <+> pretty cn <+> pretty vn
              Nothing -> fn <+> "_ = Nothing"
            clauses = normalClauses ++ [fallThroughClause]
         in hardvcat clauses

  let lspEnumD =
        let
          baseTypeD = "type EnumBaseType" <+> pretty tn <+> "=" <+> ty
          decls = [knownValuesD, baseTypeD, toBaseTypeD] ++ if custom then [] else [fromBaseTypeD]
          instanceDoc = "instance" <+> lspEnumN <+> pretty tn <+> "where" <> nest indentSize (hardline <> vcat decls)
         in
          instanceDoc
  let lspOpenEnumD = "instance" <+> lspOpenEnumN <+> pretty tn <+> "where" <> nest indentSize (hardline <> fromBaseTypeD)

  pure $
    dataD
      <> hardline
      <> hardline
      <> lspEnumD
      <> hardline
      <> hardline
      <> (if custom then lspOpenEnumD <> hardline <> hardline else "")

genAlias :: TypeAlias -> CodeGenM T.Text
genAlias a@TypeAlias{name} = do
  st <- asks symbolTable
  hsName <- case Map.lookup name st of
    Just hsn -> pure hsn
    Nothing -> fail $ "Unknown type: " <> show name
  genModule (typesModSegment <> "." <> hsName) [] Nothing (printAlias hsName a)

printAlias :: forall ann. T.Text -> TypeAlias -> ModuleGenM (Doc ann)
printAlias hsName TypeAlias{name, type_, documentation, since, proposed, deprecated} = do
  st <- asks symbolTable
  rhs <- convertType type_

  ensureImport "GHC.Generics" Unqual
  ensureImport "Control.DeepSeq" Unqual
  ensureImport "Data.Hashable" Unqual
  ensureImport "Data.Aeson" (QualAs "Aeson")
  ensureImport "Data.Row.Aeson" (QualAs "Aeson")
  ensureImport "Data.Row.Hashable" (QualAs "Hashable")
  ensureImport "Prettyprinter" Unqual
  ensureImport "Language.LSP.Protocol.Utils.Misc" Unqual
  -- In practice, it seems that only base types and aliases to base types get used as map keys, so deriving
  -- To/FromJSONKey for them seems to be enough
  let derivDoc =
        let aesonDeriving :: [Doc ann] = ["Aeson.ToJSON", "Aeson.FromJSON"] ++ case type_ of BaseType _ -> ["Aeson.ToJSONKey", "Aeson.FromJSONKey"]; _ -> []
            newtypeDeriv = "deriving newtype" <+> tupled aesonDeriving
            stockDeriv = "deriving stock" <+> tupled (fmap pretty toStockDerive)
            anyclassDeriv = "deriving anyclass" <+> tupled (fmap pretty toAnyclassDerive)
            viaDeriv = "deriving" <+> "Pretty" <+> "via" <+> parens ("ViaJSON" <+> pretty hsName)
         in indent indentSize $ hardvcat [newtypeDeriv, stockDeriv, anyclassDeriv, viaDeriv]
  dataDoc <- multilineHaddock . pretty <$> mkDocumentation documentation since proposed
  let dataDecl = "newtype" <+> pretty hsName <+> "=" <+> pretty hsName <+> rhs
      datad = hardvcat (optDeprecated hsName deprecated ++ [dataDoc, dataDecl, derivDoc])
  pure datad

---------------

data RequestData ann = RequestData
  { methCon :: Doc ann
  , singCon :: Doc ann
  , paramsEq :: Doc ann
  , resultEq :: Doc ann
  , errorDataEq :: Doc ann
  , registrationOptionsEq :: Doc ann
  , toStringClause :: Doc ann
  , fromStringClause :: Doc ann
  , messageDirectionClause :: Doc ann
  , messageKindClause :: Doc ann
  }

data NotificationData ann = NotificationData
  { methCon :: Doc ann
  , singCon :: Doc ann
  , paramsEq :: Doc ann
  , registrationOptionsEq :: Doc ann
  , toStringClause :: Doc ann
  , fromStringClause :: Doc ann
  , messageDirectionClause :: Doc ann
  , messageKindClause :: Doc ann
  }

data CustomData ann = CustomData
  { methCon :: Doc ann
  , singCon :: Doc ann
  , paramsEq :: Doc ann
  , resultEq :: Doc ann
  , errorDataEq :: Doc ann
  , registrationOptionsEq :: Doc ann
  , toStringClause :: Doc ann
  , fromStringClause :: Doc ann
  , messageDirectionClause :: Doc ann
  , messageKindClause :: Doc ann
  }

-- See Note [Generating code for methods]
-- TODO: partial result params
printMethods :: [Request] -> [Notification] -> ModuleGenM (Doc ann)
printMethods reqs nots = do
  let mtyN = "Method"
      styN = "SMethod"
      sstyN = "SomeMethod"
      smcn = "SomeMethod"
      mpN = "MessageParams"
      mrN = "MessageResult"
      edN = "ErrorData"
      roN = "RegistrationOptions"
      toStringN = "someMethodToMethodString"
      fromStringN = "methodStringToSomeMethod"
      mdN = "messageDirection"
      mkN = "messageKind"

  let methodName context fullName =
        let pieces = T.splitOn "/" fullName
         in pretty $ makeConstrName context $ foldMap capitalize pieces
  let messagePartType t = case t of
        Just ty -> convertType ty
        -- See Note [Absent parameters/results/errors]
        Nothing -> do
          ensureImport "Data.Void" Qual
          pure "Maybe Data.Void.Void"

  ensureImport "Language.LSP.Protocol.Message.Meta" (QualAs "MM")

  -- Construct the various pieces we'll need for the declarations in one go
  reqData <- for reqs $ \Request{method, params, result, errorData, registrationOptions, messageDirection} -> do
    -- <constructor name> :: Method <direction> <method type>
    let mcn = methodName (Just mtyN) method
        direction = case messageDirection of
          MM.ClientToServer -> "MM.ClientToServer"
          MM.ServerToClient -> "MM.ServerToClient"
          MM.Both -> "f"
        methCon = mcn <+> "::" <+> pretty mtyN <+> direction <+> "MM.Request"
        scn = methodName (Just styN) method
        singCon = scn <+> "::" <+> pretty styN <+> mcn

    -- MessageParams <constructor name> = <param type>
    paramTy <- messagePartType params
    let paramsEq = mpN <+> mcn <+> "=" <+> paramTy
    -- MessageResult <constructor name> = <result type>
    resultTy <- messagePartType (Just result)
    let resultEq = mrN <+> mcn <+> "=" <+> resultTy
    errDatTy <- messagePartType errorData
    let errorDataEq = edN <+> mcn <+> "=" <+> errDatTy
    regOptsTy <- messagePartType registrationOptions
    let registrationOptionsEq = roN <+> mcn <+> "=" <+> regOptsTy

    let toStringClause = toStringN <+> parens (smcn <+> scn) <+> "=" <+> dquotes (pretty method)
        fromStringClause = fromStringN <+> dquotes (pretty method) <+> "=" <+> smcn <+> scn
        messageDirectionClause =
          let d = case messageDirection of
                MM.ClientToServer -> "MM.SClientToServer"
                MM.ServerToClient -> "MM.SServerToClient"
                MM.Both -> "MM.SBothDirections"
           in mdN <+> scn <+> "=" <+> d
        messageKindClause = "messageKind" <+> scn <+> "=" <+> "MM.SRequest"
    pure $ RequestData{..}

  notData <- for nots $ \Notification{method, params, registrationOptions, messageDirection} -> do
    let mcn = methodName (Just mtyN) method
        direction = case messageDirection of
          MM.ClientToServer -> "MM.ClientToServer"
          MM.ServerToClient -> "MM.ServerToClient"
          MM.Both -> "f"
        methCon = mcn <+> "::" <+> pretty mtyN <+> direction <+> "MM.Notification"
        scn = methodName (Just styN) method
        singCon = scn <+> "::" <+> pretty styN <+> mcn

    -- MessageParams <constructor name> = <param type>
    paramTy <- messagePartType params
    let paramsEq = mpN <+> mcn <+> "=" <+> paramTy
    regOptsTy <- messagePartType registrationOptions
    let registrationOptionsEq = roN <+> mcn <+> "=" <+> regOptsTy

    let toStringClause = toStringN <+> parens (smcn <+> scn) <+> "=" <+> dquotes (pretty method)
        fromStringClause = fromStringN <+> dquotes (pretty method) <+> "=" <+> smcn <+> scn
        messageDirectionClause =
          let d = case messageDirection of
                MM.ClientToServer -> "MM.SClientToServer"
                MM.ServerToClient -> "MM.SServerToClient"
                MM.Both -> "MM.SBothDirections"
           in "messageDirection" <+> scn <+> "=" <+> d
        messageKindClause = "messageKind" <+> scn <+> "=" <+> "MM.SNotification"

    pure $ NotificationData{..}

  -- Add the custom method case, which isn't in the metamodel
  customDat <- do
    let mcn = methodName (Just mtyN) "CustomMethod"
        -- Method_CustomMethod :: Symbol -> Method f t
        methCon = mcn <+> "::" <+> "GHC.TypeLits.Symbol" <+> "->" <+> pretty mtyN <+> "f" <+> "t"
        -- SMethod_CustomMethod :: KnownSymbol s => SMethod Method_CustomMethod
        scn = methodName (Just styN) "CustomMethod"
    ensureImport "Data.Proxy" Qual
    ensureImport "GHC.TypeLits" Qual
    let singCon = scn <+> "::" <+> "forall s . GHC.TypeLits.KnownSymbol s =>" <+> "Data.Proxy.Proxy s" <+> "->" <+> pretty styN <+> parens (mcn <+> "s")
    -- MessageParams (Method_CustomMethod s) = Value
    ensureImport "Data.Aeson" (QualAs "Aeson")
    let paramsEq = mpN <+> parens (mcn <+> "s") <+> "=" <+> "Aeson.Value"
        -- MessageResult (Method_CustomMethod s) = Value
        resultEq = mrN <+> parens (mcn <+> "s") <+> "=" <+> "Aeson.Value"
        -- Can shove whatever you want in the error data for custom methods?
        -- ErrorData (Method_CustomMethod s) = Value
        errorDataEq = edN <+> parens (mcn <+> "s") <+> "=" <+> "Aeson.Value"
    -- Can't register custom methods
    -- RegistrationOptions (Method_CustomMethod s) = Void
    ensureImport "Data.Void" Qual
    let registrationOptionsEq = roN <+> parens (mcn <+> "s") <+> "=" <+> "Data.Void.Void"

    let toStringClause = toStringN <+> parens (smcn <+> parens (scn <+> "v")) <+> "=" <+> "GHC.TypeLits.symbolVal v"
        fromStringClause = fromStringN <+> "v = case GHC.TypeLits.someSymbolVal v of { GHC.TypeLits.SomeSymbol p ->" <+> smcn <+> parens (scn <+> "p") <+> "; }"
        messageDirectionClause = mdN <+> parens (scn <+> "_") <+> "=" <+> "MM.SBothDirections"
        messageKindClause = mkN <+> parens (scn <+> "_") <+> "=" <+> "MM.SBothTypes"

    pure $ CustomData{..}

  ensureImport "Data.Kind" (QualAs "Kind")
  let dataD =
        let sigD = "type" <+> pretty mtyN <+> ":: MM.MessageDirection -> MM.MessageKind -> Kind.Type"
            docD = "-- | A type representing a LSP method (or class of methods), intended to be used mostly at the type level."
            ctors = fmap (\RequestData{..} -> methCon) reqData ++ fmap (\NotificationData{..} -> methCon) notData ++ [(\CustomData{..} -> methCon) customDat]
            dataD = nest indentSize $ "data" <+> pretty mtyN <+> "f t" <+> "where" <+> (hardline <> hardvcat ctors)
         in -- This only really exists on the type level so we don't really want instances anyway
            hardvcat [docD, sigD, dataD]

  let mpD =
        let sigD = "type" <+> mpN <+> ":: forall f t ." <+> pretty mtyN <+> "f t" <+> "->" <+> "Kind.Type"
            docD = "-- | Maps a LSP method to its parameter type."
            eqns = fmap (\RequestData{..} -> paramsEq) reqData ++ fmap (\NotificationData{..} -> paramsEq) notData ++ [(\CustomData{..} -> paramsEq) customDat]
            declD = nest indentSize $ "type family" <+> mpN <+> parens ("m :: " <+> pretty mtyN <+> "f t") <+> "where" <+> (hardline <> hardvcat eqns)
         in hardvcat [docD, sigD, declD]

  let mrD =
        let sigD = "type" <+> mrN <+> ":: forall f t ." <+> pretty mtyN <+> "f t" <+> "->" <+> "Kind.Type"
            docD = "-- | Maps a LSP method to its result type."
            -- TODO: should we give notifiations ()?
            eqns = fmap (\RequestData{..} -> resultEq) reqData ++ [(\CustomData{..} -> resultEq) customDat]
            declD = nest indentSize $ "type family" <+> mrN <+> parens ("m :: " <+> pretty mtyN <+> "f t") <+> "where" <+> (hardline <> hardvcat eqns)
         in hardvcat [docD, sigD, declD]

  let edD =
        let sigD = "type" <+> edN <+> ":: forall f t ." <+> pretty mtyN <+> "f t" <+> "->" <+> "Kind.Type"
            docD = "-- | Maps a LSP method to its error data type."
            -- TODO: should we give notifiations ()?
            eqns = fmap (\RequestData{..} -> errorDataEq) reqData ++ [(\CustomData{..} -> errorDataEq) customDat]
            declD = nest indentSize $ "type family" <+> edN <+> parens ("m :: " <+> pretty mtyN <+> "f t") <+> "where" <+> (hardline <> hardvcat eqns)
         in hardvcat [docD, sigD, declD]

  let roD =
        let sigD = "type" <+> roN <+> ":: forall f t ." <+> pretty mtyN <+> "f t" <+> "->" <+> "Kind.Type"
            docD = "-- | Maps a LSP method to its registration options type."
            eqns = fmap (\RequestData{..} -> registrationOptionsEq) reqData ++ fmap (\NotificationData{..} -> registrationOptionsEq) notData ++ [(\CustomData{..} -> registrationOptionsEq) customDat]
            declD = nest indentSize $ "type family" <+> roN <+> parens ("m :: " <+> pretty mtyN <+> "f t") <+> "where" <+> (hardline <> hardvcat eqns)
         in hardvcat [docD, sigD, declD]

  let singD =
        let sigD = "type" <+> pretty styN <+> ":: forall f t ." <+> pretty mtyN <+> "f t" <+> "->" <+> "Kind.Type"
            docD = "-- | A singleton type for 'Method'."
            ctors = fmap (\RequestData{..} -> singCon) reqData ++ fmap (\NotificationData{..} -> singCon) notData ++ [(\CustomData{..} -> singCon) customDat]
            -- Can't derive instances, it's a GADT, will do them later
            dataD = nest indentSize $ "data" <+> pretty styN <+> "m" <+> "where" <+> (hardline <> hardvcat ctors)
         in hardvcat [docD, sigD, dataD]

  let ssmD =
        let ctor = smcn <+> "::" <+> "forall m ." <+> pretty styN <+> "m" <+> "->" <+> sstyN
            docD = "-- | A method which isn't statically known."
            -- Can't derive instances because it's a GADT and we're not doing the instances for SMethod here either
            dataD = nest indentSize $ "data" <+> sstyN <+> "where" <+> (hardline <> ctor)
         in hardvcat [docD, dataD]

  -- methodToString :: SomeMethod -> String
  let toStringD =
        let docD = "-- | Turn a 'SomeMethod' into its LSP method string."
            sigD = toStringN <+> "::" <+> sstyN <+> "->" <+> "String"
            clauses = fmap (\RequestData{..} -> toStringClause) reqData ++ fmap (\NotificationData{..} -> toStringClause) notData ++ [(\CustomData{..} -> toStringClause) customDat]
         in hardvcat [docD, sigD, hardvcat clauses]
  -- stringToMethod :: String -> SomeMethod
  let fromStringD =
        let docD = "-- | Turn a LSP method string into a 'SomeMethod'."
            sigD = fromStringN <+> "::" <+> "String" <+> "->" <+> sstyN
            clauses = fmap (\RequestData{..} -> fromStringClause) reqData ++ fmap (\NotificationData{..} -> fromStringClause) notData ++ [(\CustomData{..} -> fromStringClause) customDat]
         in hardvcat [docD, sigD, hardvcat clauses]

  let messageDirectionD =
        let docD = "-- | Get a singleton witness for the message direction of a 'SMethod'."
            sigD = mdN <+> ":: forall f t (m :: Method f t) ." <+> pretty styN <+> "m" <+> "->" <+> "MM.SMessageDirection f"
            clauses = fmap (\RequestData{..} -> messageDirectionClause) reqData ++ fmap (\NotificationData{..} -> messageDirectionClause) notData ++ [(\CustomData{..} -> messageDirectionClause) customDat]
         in hardvcat [docD, sigD, hardvcat clauses]

  let messageKindD =
        let docD = "-- | Get a singleton witness for the message kind of a 'SMethod'."
            sigD = mkN <+> ":: forall f t (m :: Method f t) ." <+> pretty styN <+> "m" <+> "->" <+> "MM.SMessageKind t"
            clauses = fmap (\RequestData{..} -> messageKindClause) reqData ++ fmap (\NotificationData{..} -> messageKindClause) notData ++ [(\CustomData{..} -> messageKindClause) customDat]
         in hardvcat [docD, sigD, hardvcat clauses]

  pure $
    dataD
      <> hardline
      <> hardline
      <> mpD
      <> hardline
      <> hardline
      <> mrD
      <> hardline
      <> hardline
      <> edD
      <> hardline
      <> hardline
      <> roD
      <> hardline
      <> hardline
      <> singD
      <> hardline
      <> hardline
      <> ssmD
      <> hardline
      <> hardline
      <> toStringD
      <> hardline
      <> hardline
      <> fromStringD
      <> hardline
      <> hardline
      <> messageDirectionD
      <> hardline
      <> hardline
      <> messageKindD

genMethods :: [Request] -> [Notification] -> CodeGenM T.Text
genMethods reqs nots = do
  genModule "Method" [] Nothing (printMethods reqs nots)

---------------

genMetaModule :: [T.Text] -> [T.Text] -> [T.Text] -> CodeGenM T.Text
genMetaModule structNames aliasNames enumNames = do
  genModule "Meta" ["TemplateHaskell"] Nothing $ do
    ensureImport "Language.Haskell.TH" (QualAs "TH")
    let tyn thn = pretty <$> entityName "Language.LSP.Protocol.Internal.Types" thn
    sns <- traverse tyn structNames
    ans <- traverse tyn aliasNames
    ens <- traverse tyn enumNames
    let
      sig1 = "structNames" <+> "::" <+> brackets "TH.Name"
      decl1 = "structNames =" <+> nest indentSize (encloseSep "[" "]" "," $ fmap (\n -> "''" <> n) sns)
      sig2 = "aliasNames" <+> "::" <+> brackets "TH.Name"
      decl2 = "aliasNames =" <+> nest indentSize (encloseSep "[" "]" "," $ fmap (\n -> "''" <> n) ans)
      sig3 = "enumNames" <+> "::" <+> brackets "TH.Name"
      decl3 = "enumNames =" <+> nest indentSize (encloseSep "[" "]" "," $ fmap (\n -> "''" <> n) ens)
    pure $
      hardvcat
        [ sig1
        , decl1
        , sig2
        , decl2
        , sig3
        , decl3
        ]

---------------

printReExports :: [T.Text] -> ModuleGenM (Doc ann)
printReExports names = do
  for_ names $ \n -> ensureImport n (As "Export")
  pure mempty

genAllModule :: [T.Text] -> CodeGenM T.Text
genAllModule names = do
  genModule typesModSegment [] (Just ["module Export"]) (printReExports names)

---------------

capitalize :: T.Text -> T.Text
capitalize s = T.toUpper (T.singleton (T.head s)) `T.append` T.tail s

uncapitalize :: T.Text -> T.Text
uncapitalize s = T.toLower (T.singleton (T.head s)) `T.append` T.tail s

hardvcat :: [Doc ann] -> Doc ann
hardvcat = concatWith (\x y -> x <> hardline <> y)

mkIterApplicativeApp :: Doc a -> [Doc a] -> Doc a
mkIterApplicativeApp hd [] = "pure" <+> hd
mkIterApplicativeApp hd (a : rest) =
  let acc = hd <+> "<$>" <+> a
   in foldl' (\acc a -> acc <+> "<*>" <+> a) acc rest

{- Note [Code generation approach]
The approach we take here is quite primitive: we just print out Haskell modules
as strings. This ends up being better than the alternatives!

Using TH:
- Hard to make it work reliably on all GHC versions
- Have to produce everything in a single module
- Slow compilation: the TH itself is slow, and then it produces a gigantic module
  which must be compiled in one go
- Hard to debug: you have to dump splices and dig through the output

Various other libraries for generating Haskell:
- Only support old versions of Haskell syntax (we need GADTs and type families)
- Are dubiously supported
-}

{- Note [Translating metamodel types]

= Or types

Or types are translated directly into anonymous unions using '(|?)'.

= And types

And types are difficult to handle in general (it's not even clear what that means). We assume
that they contain only references to structures, and translate them as anonymous records
with the union of the fields of the components of the and type.

= Null

We would like a type that reliably serializes to/from null, since null alternatives
are called out explicitly in the LSP spec. In the end, we just defined a specific type for
this: 'Null'.

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

Structure literals are translated directly as anonymous records. See Note [Anonymous records].

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
- Field names clash a lot
- Constructor names clash
- There are a few instances where constructor names clash with type names.

One approach would be to generate lots of modules and use Haskell's module system
to disambiguate. But this would prevent us from providing large modules that
re-export things, rather we would need users to import each module that they
use individually, which would be quite tedious. That would also force us to
expose the generated module structure.

The main thing we do is just pick non-clashing names. The crude heuristic
we have adopted is to prefix many values with the name of the type with which they
are associated, followed by an underscore. So the constructors of `X` will be
`X_A`, `X_B` etc.

We don't do this for fields, instead we rely on `DuplicateRecordFields` and
use classy lenses.
-}

{- Note [Principle of robustness for parsing LSP types]
The principle of robustness states:

> Be conservative in what you do, liberal in what you accept from others

We try to follow this when parsing LSP types, and where possible accept
"slightly wrong" input. This is important because the LSP spec is very
fiddly to implement correctly, and there are many clients we interact with,
some of whom will therefore get it wrong. It's best if we can accept this
(although in an ideal world we would also emit a warning, but it's not super
easy to do that, so we just don't).

Specific ways in which we try to be robust:
- Accept 'null' to mean "missing". The LSP spec *sometimes* allows a value
  to be 'null', but often it says a value can be missing but is not nullable.
  A common mistake for clients is to still send 'null' to mean "missing"
  (see e.g. https://github.com/haskell/haskell-language-server/issues/3842#issuecomment-1798217080).
  This is complicated because if 'null' is an allowed value then we want to parse
  it as a present 'null', not missing.
-}
