{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.LSP.Types.SemanticTokens where

import qualified Data.Aeson                                   as A
import           Data.Aeson.TH
import           Data.Text                                    (Text)

import           Control.Monad.Except

import           Language.LSP.Types.Common
import           Language.LSP.Types.Location
import           Language.LSP.Types.Progress
import           Language.LSP.Types.StaticRegistrationOptions
import           Language.LSP.Types.TextDocument
import           Language.LSP.Types.Utils

import qualified Data.Algorithm.Diff                          as Diff
import qualified Data.Bits                                    as Bits
import qualified Data.DList                                   as DList
import           Data.Default
import           Data.Foldable                                hiding (length)
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (fromMaybe,
                                                               maybeToList)
import           Data.String

data SemanticTokenTypes =
  SttType
  | SttClass
  | SttEnum
  | SttInterface
  | SttStruct
  | SttTypeParameter
  | SttParameter
  | SttVariable
  | SttProperty
  | SttEnumMember
  | SttEvent
  | SttFunction
  | SttMethod
  | SttMacro
  | SttKeyword
  | SttModifier
  | SttComment
  | SttString
  | SttNumber
  | SttRegexp
  | SttOperator
  | SttUnknown Text
  deriving (Show, Read, Eq, Ord)

instance A.ToJSON SemanticTokenTypes where
  toJSON SttType          = A.String "type"
  toJSON SttClass         = A.String "class"
  toJSON SttEnum          = A.String "enum"
  toJSON SttInterface     = A.String "interface"
  toJSON SttStruct        = A.String "struct"
  toJSON SttTypeParameter = A.String "typeParameter"
  toJSON SttParameter     = A.String "parameter"
  toJSON SttVariable      = A.String "variable"
  toJSON SttProperty      = A.String "property"
  toJSON SttEnumMember    = A.String "enumMember"
  toJSON SttEvent         = A.String "event"
  toJSON SttFunction      = A.String "function"
  toJSON SttMethod        = A.String "method"
  toJSON SttMacro         = A.String "macro"
  toJSON SttKeyword       = A.String "keyword"
  toJSON SttModifier      = A.String "modifier"
  toJSON SttComment       = A.String "comment"
  toJSON SttString        = A.String "string"
  toJSON SttNumber        = A.String "number"
  toJSON SttRegexp        = A.String "regexp"
  toJSON SttOperator      = A.String "operator"
  toJSON (SttUnknown t)   = A.String t

instance A.FromJSON SemanticTokenTypes where
  parseJSON (A.String "type")          = pure SttType
  parseJSON (A.String "class")         = pure SttClass
  parseJSON (A.String "enum")          = pure SttEnum
  parseJSON (A.String "interface")     = pure SttInterface
  parseJSON (A.String "struct")        = pure SttStruct
  parseJSON (A.String "typeParameter") = pure SttTypeParameter
  parseJSON (A.String "parameter")     = pure SttParameter
  parseJSON (A.String "variable")      = pure SttVariable
  parseJSON (A.String "property")      = pure SttProperty
  parseJSON (A.String "enumMember")    = pure SttEnumMember
  parseJSON (A.String "event")         = pure SttEvent
  parseJSON (A.String "function")      = pure SttFunction
  parseJSON (A.String "method")        = pure SttMethod
  parseJSON (A.String "macro")         = pure SttMacro
  parseJSON (A.String "keyword")       = pure SttKeyword
  parseJSON (A.String "modifier")      = pure SttModifier
  parseJSON (A.String "comment")       = pure SttComment
  parseJSON (A.String "string")        = pure SttString
  parseJSON (A.String "number")        = pure SttNumber
  parseJSON (A.String "regexp")        = pure SttRegexp
  parseJSON (A.String "operator")      = pure SttOperator
  parseJSON (A.String t)               = pure $ SttUnknown t
  parseJSON _                          = mempty

-- | The set of semantic token types which are "known" (i.e. listed in the LSP spec).
knownSemanticTokenTypes :: [SemanticTokenTypes]
knownSemanticTokenTypes = [
  SttType
  , SttClass
  , SttEnum
  , SttInterface
  , SttStruct
  , SttTypeParameter
  , SttParameter
  , SttVariable
  , SttProperty
  , SttEnumMember
  , SttEvent
  , SttFunction
  , SttMethod
  , SttMacro
  , SttKeyword
  , SttModifier
  , SttComment
  , SttString
  , SttNumber
  , SttRegexp
  , SttOperator
  ]

data SemanticTokenModifiers =
  StmDeclaration
  | StmDefinition
  | StmReadonly
  | StmStatic
  | StmDeprecated
  | StmAbstract
  | StmAsync
  | StmModification
  | StmDocumentation
  | StmDefaultLibrary
  | StmUnknown Text
  deriving (Show, Read, Eq, Ord)

instance A.ToJSON SemanticTokenModifiers where
  toJSON StmDeclaration    = A.String "declaration"
  toJSON StmDefinition     = A.String "definition"
  toJSON StmReadonly       = A.String "readonly"
  toJSON StmStatic         = A.String "static"
  toJSON StmDeprecated     = A.String "deprecated"
  toJSON StmAbstract       = A.String "abstract"
  toJSON StmAsync          = A.String "async"
  toJSON StmModification   = A.String "modification"
  toJSON StmDocumentation  = A.String "documentation"
  toJSON StmDefaultLibrary = A.String "defaultLibrary"
  toJSON (StmUnknown t)    = A.String t

instance A.FromJSON SemanticTokenModifiers where
  parseJSON (A.String "declaration")    = pure StmDeclaration
  parseJSON (A.String "definition")     = pure StmDefinition
  parseJSON (A.String "readonly")       = pure StmReadonly
  parseJSON (A.String "static")         = pure StmStatic
  parseJSON (A.String "deprecated")     = pure StmDeprecated
  parseJSON (A.String "abstract")       = pure StmAbstract
  parseJSON (A.String "async")          = pure StmAsync
  parseJSON (A.String "modification")   = pure StmModification
  parseJSON (A.String "documentation")  = pure StmDocumentation
  parseJSON (A.String "defaultLibrary") = pure StmDefaultLibrary
  parseJSON (A.String t)                = pure $ StmUnknown t
  parseJSON _                           = mempty

-- | The set of semantic token modifiers which are "known" (i.e. listed in the LSP spec).
knownSemanticTokenModifiers :: [SemanticTokenModifiers]
knownSemanticTokenModifiers = [
  StmDeclaration
  , StmDefinition
  , StmReadonly
  , StmStatic
  , StmDeprecated
  , StmAbstract
  , StmAsync
  , StmModification
  , StmDocumentation
  , StmDefaultLibrary
  ]

data TokenFormat = TokenFormatRelative
  deriving (Show, Read, Eq)

instance A.ToJSON TokenFormat where
  toJSON TokenFormatRelative = A.String "relative"

instance A.FromJSON TokenFormat where
  parseJSON (A.String "relative") = pure TokenFormatRelative
  parseJSON _                     = mempty

data SemanticTokensLegend = SemanticTokensLegend {
  -- | The token types a server uses.
  _tokenTypes     :: List SemanticTokenTypes,
  -- | The token modifiers a server uses.
  _tokenModifiers :: List SemanticTokenModifiers
} deriving (Show, Read, Eq)
deriveJSON lspOptions ''SemanticTokensLegend

-- We give a default legend which just lists the "known" types and modifiers in the order they're listed.
instance Default SemanticTokensLegend where
  def = SemanticTokensLegend (List knownSemanticTokenTypes) (List knownSemanticTokenModifiers)

data SemanticTokensRangeClientCapabilities = SemanticTokensRangeBool Bool | SemanticTokensRangeObj A.Value
  deriving (Show, Read, Eq)
deriveJSON lspOptionsUntagged ''SemanticTokensRangeClientCapabilities

data SemanticTokensDeltaClientCapabilities = SemanticTokensDeltaClientCapabilities {
  -- | The client will send the `textDocument/semanticTokens/full/delta`
  -- request if the server provides a corresponding handler.
  _delta :: Maybe Bool
} deriving (Show, Read, Eq)
deriveJSON lspOptions ''SemanticTokensDeltaClientCapabilities

data SemanticTokensFullClientCapabilities = SemanticTokensFullBool Bool | SemanticTokensFullDelta SemanticTokensDeltaClientCapabilities
  deriving (Show, Read, Eq)
deriveJSON lspOptionsUntagged ''SemanticTokensFullClientCapabilities

data SemanticTokensRequestsClientCapabilities = SemanticTokensRequestsClientCapabilities {
  -- | The client will send the `textDocument/semanticTokens/range` request
  -- if the server provides a corresponding handler.
  _range :: Maybe SemanticTokensRangeClientCapabilities,
  -- | The client will send the `textDocument/semanticTokens/full` request
  -- if the server provides a corresponding handler.
  _full  :: Maybe SemanticTokensFullClientCapabilities
} deriving (Show, Read, Eq)
deriveJSON lspOptions ''SemanticTokensRequestsClientCapabilities

data SemanticTokensClientCapabilities = SemanticTokensClientCapabilities {
  -- | Whether implementation supports dynamic registration. If this is set to
  -- `true` the client supports the new `(TextDocumentRegistrationOptions &
  -- StaticRegistrationOptions)` return value for the corresponding server
  -- capability as well.
  _dynamicRegistration     :: Maybe Bool,

  -- | Which requests the client supports and might send to the server
  -- depending on the server's capability. Please note that clients might not
  -- show semantic tokens or degrade some of the user experience if a range
  -- or full request is advertised by the client but not provided by the
  -- server. If for example the client capability `requests.full` and
  -- `request.range` are both set to true but the server only provides a
  -- range provider the client might not render a minimap correctly or might
  -- even decide to not show any semantic tokens at all.
  _requests                :: SemanticTokensRequestsClientCapabilities,

  -- | The token types that the client supports.
  _tokenTypes              :: List SemanticTokenTypes,

  -- | The token modifiers that the client supports.
  _tokenModifiers          :: List SemanticTokenModifiers,

  -- | The formats the clients supports.
  _formats                 :: List TokenFormat,

  -- | Whether the client supports tokens that can overlap each other.
  _overlappingTokenSupport :: Maybe Bool,

  -- | Whether the client supports tokens that can span multiple lines.
  _multilineTokenSupport   :: Maybe Bool
} deriving (Show, Read, Eq)
deriveJSON lspOptions ''SemanticTokensClientCapabilities

makeExtendingDatatype "SemanticTokensOptions" [''WorkDoneProgressOptions]
  [ ("_legend", [t| SemanticTokensLegend |])
  , ("_range", [t| Maybe SemanticTokensRangeClientCapabilities |])
  , ("_full", [t| Maybe SemanticTokensFullClientCapabilities |])
  ]
deriveJSON lspOptions ''SemanticTokensOptions

makeExtendingDatatype "SemanticTokensRegistrationOptions"
  [ ''TextDocumentRegistrationOptions
  , ''SemanticTokensOptions
  , ''StaticRegistrationOptions] []
deriveJSON lspOptions ''SemanticTokensRegistrationOptions

makeExtendingDatatype "SemanticTokensParams"
  [''WorkDoneProgressParams
  , ''PartialResultParams]
  [ ("_textDocument", [t| TextDocumentIdentifier |]) ]
deriveJSON lspOptions ''SemanticTokensParams

data SemanticTokens = SemanticTokens {
  -- | An optional result id. If provided and clients support delta updating
  -- the client will include the result id in the next semantic token request.
  -- A server can then instead of computing all semantic tokens again simply
  -- send a delta.
  _resultId :: Maybe Text,

  -- | The actual tokens.
  _xdata    :: List Word32
} deriving (Show, Read, Eq)
deriveJSON lspOptions ''SemanticTokens

data SemanticTokensPartialResult = SemanticTokensPartialResult {
  _xdata :: List Word32
}
deriveJSON lspOptions ''SemanticTokensPartialResult

makeExtendingDatatype "SemanticTokensDeltaParams"
  [''WorkDoneProgressParams
  , ''PartialResultParams]
  [ ("_textDocument", [t| TextDocumentIdentifier  |])
  , ("_previousResultId", [t| Text |])
  ]
deriveJSON lspOptions ''SemanticTokensDeltaParams

data SemanticTokensEdit = SemanticTokensEdit {
  -- | The start offset of the edit.
  _start       :: Word32,
  -- | The count of elements to remove.
  _deleteCount :: Word32,
  -- | The elements to insert.
  _xdata       :: Maybe (List Word32)
} deriving (Show, Read, Eq)
deriveJSON lspOptions ''SemanticTokensEdit

data SemanticTokensDelta = SemanticTokensDelta {
  _resultId :: Maybe Text,
  -- | The semantic token edits to transform a previous result into a new
  -- result.
  _edits    :: List SemanticTokensEdit
} deriving (Show, Read, Eq)
deriveJSON lspOptions ''SemanticTokensDelta

data SemanticTokensDeltaPartialResult = SemantictokensDeltaPartialResult {
  _edits :: List SemanticTokensEdit
} deriving (Show, Read, Eq)
deriveJSON lspOptions ''SemanticTokensDeltaPartialResult

makeExtendingDatatype "SemanticTokensRangeParams"
  [''WorkDoneProgressParams
  , ''PartialResultParams]
  [ ("_textDocument", [t| TextDocumentIdentifier  |])
  , ("_range", [t| Range |])
  ]
deriveJSON lspOptions ''SemanticTokensRangeParams

data SemanticTokensWorkspaceClientCapabilities = SemanticTokensWorkspaceClientCapabilities {
  -- | Whether the client implementation supports a refresh request sent from
  -- the server to the client.
  --
  -- Note that this event is global and will force the client to refresh all
  -- semantic tokens currently shown. It should be used with absolute care
  -- and is useful for situation where a server for example detect a project
  -- wide change that requires such a calculation.
  _refreshSupport :: Maybe Bool
} deriving (Show, Read, Eq)
deriveJSON lspOptions ''SemanticTokensWorkspaceClientCapabilities

----------------------------------------------------------
-- Tools for working with semantic tokens.
----------------------------------------------------------

-- | A single 'semantic token' as described in the LSP specification, using absolute positions.
-- This is the kind of token that is usually easiest for editors to produce.
data SemanticTokenAbsolute = SemanticTokenAbsolute {
  line           :: Word32,
  startChar      :: Word32,
  length         :: Word32,
  tokenType      :: SemanticTokenTypes,
  tokenModifiers :: [SemanticTokenModifiers]
} deriving (Show, Read, Eq, Ord)
-- Note: we want the Ord instance to sort the tokens textually: this is achieved due to the
-- order of the constructors

-- | A single 'semantic token' as described in the LSP specification, using relative positions.
data SemanticTokenRelative = SemanticTokenRelative {
  deltaLine      :: Word32,
  deltaStartChar :: Word32,
  length         :: Word32,
  tokenType      :: SemanticTokenTypes,
  tokenModifiers :: [SemanticTokenModifiers]
} deriving (Show, Read, Eq, Ord)
-- Note: we want the Ord instance to sort the tokens textually: this is achieved due to the
-- order of the constructors

-- | Turn a list of absolutely-positioned tokens into a list of relatively-positioned tokens. The tokens are assumed to be in the
-- order that they appear in the document!
relativizeTokens :: [SemanticTokenAbsolute] -> [SemanticTokenRelative]
relativizeTokens xs = DList.toList $ go 0 0 xs mempty
  where
    -- Pass an accumulator to make this tail-recursive
    go :: Word32 -> Word32 -> [SemanticTokenAbsolute] -> DList.DList SemanticTokenRelative -> DList.DList SemanticTokenRelative
    go _ _ [] acc = acc
    go lastLine lastChar (SemanticTokenAbsolute l c len ty mods:ts) acc =
      let
        lastCharInLine = if l == lastLine then lastChar else 0
        dl = l - lastLine
        dc = c - lastCharInLine
      in go l c ts (DList.snoc acc (SemanticTokenRelative dl dc len ty mods))

-- | Turn a list of relatively-positioned tokens into a list of absolutely-positioned tokens. The tokens are assumed to be in the
-- order that they appear in the document!
absolutizeTokens :: [SemanticTokenRelative] -> [SemanticTokenAbsolute]
absolutizeTokens xs = DList.toList $ go 0 0 xs mempty
  where
    -- Pass an accumulator to make this tail-recursive
    go :: Word32 -> Word32 -> [SemanticTokenRelative] -> DList.DList SemanticTokenAbsolute -> DList.DList SemanticTokenAbsolute
    go _ _ [] acc = acc
    go lastLine lastChar (SemanticTokenRelative dl dc len ty mods:ts) acc =
      let
        lastCharInLine = if dl == 0 then lastChar else 0
        l = lastLine + dl
        c = lastCharInLine + dc
      in go l c ts (DList.snoc acc (SemanticTokenAbsolute l c len ty mods))

-- | Encode a series of relatively-positioned semantic tokens into an integer array following the given legend.
encodeTokens :: SemanticTokensLegend -> [SemanticTokenRelative] -> Either Text [Word32]
encodeTokens SemanticTokensLegend{_tokenTypes=List tts,_tokenModifiers=List tms} sts =
  DList.toList . DList.concat <$> traverse encodeToken sts
  where
    -- Note that there's no "fast" version of these (e.g. backed by an IntMap or similar)
    -- in general, due to the possibility  of unknown token types which are only identified by strings.
    tyMap :: Map.Map SemanticTokenTypes Word32
    tyMap = Map.fromList $ zip tts [0..]
    modMap :: Map.Map SemanticTokenModifiers Int
    modMap = Map.fromList $ zip tms [0..]

    lookupTy :: SemanticTokenTypes -> Either Text Word32
    lookupTy ty = case Map.lookup ty tyMap of
        Just tycode -> pure tycode
        Nothing -> throwError $ "Semantic token type " <> fromString (show ty) <> " did not appear in the legend"
    lookupMod :: SemanticTokenModifiers -> Either Text Int
    lookupMod modifier = case Map.lookup modifier modMap of
        Just modcode -> pure modcode
        Nothing -> throwError $ "Semantic token modifier " <> fromString (show modifier) <> " did not appear in the legend"

    -- Use a DList here for better efficiency when concatenating all these together
    encodeToken :: SemanticTokenRelative -> Either Text (DList.DList Word32)
    encodeToken (SemanticTokenRelative dl dc len ty mods) = do
      tycode <- lookupTy ty
      modcodes <- traverse lookupMod mods
      let combinedModcode :: Word32 = foldl' Bits.setBit Bits.zeroBits modcodes

      pure [dl, dc, len, tycode, combinedModcode ]

-- This is basically 'SemanticTokensEdit', but slightly easier to work with.
-- | An edit to a buffer of items. 
data Edit a = Edit { editStart :: Word32, editDeleteCount :: Word32, editInsertions :: [a] }
  deriving (Read, Show, Eq, Ord)

-- | Compute a list of edits that will turn the first list into the second list.
computeEdits :: Eq a => [a] -> [a] -> [Edit a]
computeEdits l r = DList.toList $ go 0 Nothing (Diff.getGroupedDiff l r) mempty
  where
    {-
    Strategy: traverse the list of diffs, keeping the current index and (maybe) an in-progress 'Edit'.
    Whenever we see a 'Diff' that's only one side or the other, we can bundle that in to our in-progress
    'Edit'. We only have to stop if we see a 'Diff' that's on both sides (i.e. unchanged), then we
    dump the 'Edit' into the accumulator.
    We need the index, because 'Edit's need to say where they start.
    -}
    go :: Word32 -> Maybe (Edit a) -> [Diff.Diff [a]] -> DList.DList (Edit a) -> DList.DList (Edit a)
    -- No more diffs: append the current edit if there is one and return
    go _ e [] acc = acc <> DList.fromList (maybeToList e)

    -- Items only on the left (i.e. deletions): increment the current index, and record the count of deletions,
    -- starting a new edit if necessary.
    go ix e (Diff.First ds : rest) acc =
      let
        deleteCount = fromIntegral $ Prelude.length ds
        edit = fromMaybe (Edit ix 0 []) e
      in go (ix + deleteCount) (Just (edit{editDeleteCount=editDeleteCount edit + deleteCount})) rest acc
    -- Items only on the right (i.e. insertions): don't increment the current index, and record the insertions,
    -- starting a new edit if necessary.
    go ix e (Diff.Second as : rest) acc =
      let edit = fromMaybe (Edit ix 0 []) e
      in go ix (Just (edit{editInsertions=editInsertions edit <> as})) rest acc

    -- Items on both sides: increment the current index appropriately (since the items appear on the left),
    -- and append the current edit (if there is one) to our list of edits (since we can't continue it with a break).
    go ix e (Diff.Both bs _bs : rest) acc =
      let bothCount = fromIntegral $ Prelude.length bs
      in go (ix + bothCount) Nothing rest (acc <> DList.fromList (maybeToList e))

-- | Convenience method for making a 'SemanticTokens' from a list of 'SemanticTokenAbsolute's. An error may be returned if

-- The resulting 'SemanticTokens' lacks a result ID, which must be set separately if you are using that.
makeSemanticTokens :: SemanticTokensLegend -> [SemanticTokenAbsolute] -> Either Text SemanticTokens
makeSemanticTokens legend sts = do
  encoded <- encodeTokens legend $ relativizeTokens sts
  pure $ SemanticTokens Nothing (List encoded)

-- | Convenience function for making a 'SemanticTokensDelta' from a previous and current 'SemanticTokens'.
-- The resulting 'SemanticTokensDelta' lacks a result ID, which must be set separately if you are using that.
makeSemanticTokensDelta :: SemanticTokens -> SemanticTokens -> SemanticTokensDelta
makeSemanticTokensDelta SemanticTokens{_xdata=List prevTokens} SemanticTokens{_xdata=List curTokens} =
  let edits = computeEdits prevTokens curTokens
      stEdits = fmap (\(Edit s ds as) -> SemanticTokensEdit s ds (Just $ List as)) edits
  in SemanticTokensDelta Nothing (List stEdits)

