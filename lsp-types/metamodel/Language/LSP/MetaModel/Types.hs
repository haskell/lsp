{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{- |
This defines the types of the LSP "metamodel", which is a machine-readable format specifying the
types used in the LSP protocol.

The type system is quite typescript-y, which isn't surprising given that the whole protocol is
very typescript-y.

A typescript version of the metamodel types can be found here, which is useful for constructing
this Haskell model of them:
https://github.com/microsoft/vscode-languageserver-node/blob/main/tools/src/metaModel.ts
-}
module Language.LSP.MetaModel.Types where

import Data.Aeson hiding (Null, String)
import Data.Aeson qualified as JSON
import Data.Aeson.TH qualified as JSON
import Data.Char qualified as Char
import Data.Text (Text)

import Control.Lens
import Control.Monad.IO.Class
import Data.List.NonEmpty qualified as NE

import Language.Haskell.TH.Syntax (Exp, Lift (..), Q, addDependentFile)

-- | What direction is this message sent in: server to client, client to server, or both?
data MessageDirection = ServerToClient | ClientToServer | Both
  deriving stock (Show, Eq, Ord, Lift)

instance ToJSON MessageDirection where
  toJSON ServerToClient = toJSON @String "serverToClient"
  toJSON ClientToServer = toJSON @String "clientToServer"
  toJSON Both = toJSON @String "both"

instance FromJSON MessageDirection where
  parseJSON = withText "MessageDirection" $ \case
    "serverToClient" -> pure ServerToClient
    "clientToServer" -> pure ClientToServer
    "both" -> pure Both
    t -> fail $ "unknown message direction " ++ show t

-- | The "base types" in the metamodel.
data BaseTypeName = URI | DocumentUri | Integer | UInteger | Decimal | RegExp | String | Boolean | Null
  deriving stock (Show, Eq, Ord, Lift)

-- | A property of a structure.
data Property = Property
  { name :: Text
  , type_ :: Type
  , optional :: Maybe Bool
  , documentation :: Maybe Text
  , since :: Maybe Text
  , proposed :: Maybe Bool
  , deprecated :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Lift)

-- | An anonymous structure type.
data StructureLiteral = StructureLiteral
  { properties :: [Property]
  , documentation :: Maybe Text
  , since :: Maybe Text
  , proposed :: Maybe Bool
  , deprecated :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Lift)

-- | The various kinds of type in the metamodel.
data Type
  = BaseType {btName :: BaseTypeName}
  | ReferenceType {rtName :: Text}
  | ArrayType {atElement :: Type}
  | MapType {mKey :: Type, mValue :: Type}
  | AndType {aItems :: NE.NonEmpty Type}
  | OrType {oItems :: NE.NonEmpty Type}
  | TupleType {tItems :: [Type]}
  | StructureLiteralType {stlValue :: StructureLiteral}
  | StringLiteralType {slValue :: Text}
  | IntegerLiteralType {ilValue :: Integer}
  | BooleanLiteralType {blValue :: Bool}
  deriving stock (Show, Eq, Ord, Lift)

-- | A request message.
data Request = Request
  { method :: Text
  , params :: Maybe Type -- typescript says it can be [Type], but it never is so whatever
  , result :: Type
  , partialResult :: Maybe Type
  , errorData :: Maybe Type
  , registrationOptions :: Maybe Type
  , messageDirection :: MessageDirection
  , documentation :: Maybe Text
  , since :: Maybe Text
  , proposed :: Maybe Bool
  , deprecated :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Lift)

-- | A notification message.
data Notification = Notification
  { method :: Text
  , params :: Maybe Type
  , registrationOptions :: Maybe Type
  , messageDirection :: MessageDirection
  , documentation :: Maybe Text
  , since :: Maybe Text
  , proposed :: Maybe Bool
  , deprecated :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Lift)

-- | A structure type.
data Structure = Structure
  { name :: Text
  , extends :: Maybe [Type]
  , mixins :: Maybe [Type]
  , properties :: [Property]
  , documentation :: Maybe Text
  , since :: Maybe Text
  , proposed :: Maybe Bool
  , deprecated :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Lift)

-- | A type alias.
data TypeAlias = TypeAlias
  { name :: Text
  , type_ :: Type
  , documentation :: Maybe Text
  , since :: Maybe Text
  , proposed :: Maybe Bool
  , deprecated :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Lift)

{- | This is just 'string | int' on the typescript side, but
 it's convenient to have a proper type here.
-}
data TextOrInteger = T Text | I Integer
  deriving stock (Show, Eq, Ord, Lift)

-- | An entry in an enumeration.
data EnumerationEntry = EnumerationEntry
  { name :: Text
  , value :: TextOrInteger
  , documentation :: Maybe Text
  , since :: Maybe Text
  , proposed :: Maybe Bool
  , deprecated :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Lift)

-- | An enumeration type.
data Enumeration = Enumeration
  { name :: Text
  , type_ :: Type
  , values :: [EnumerationEntry]
  , supportsCustomValues :: Maybe Bool
  , documentation :: Maybe Text
  , since :: Maybe Text
  , proposed :: Maybe Bool
  , deprecated :: Maybe Text
  }
  deriving stock (Show, Eq, Ord, Lift)

-- | Metadata about the metamodel iteslf.
data MetaData = MetaData
  { version :: Text
  }
  deriving stock (Show, Eq, Ord, Lift)

-- | The entire metamodel.
data MetaModel = MetaModel
  { metaData :: MetaData
  , requests :: [Request]
  , notifications :: [Notification]
  , structures :: [Structure]
  , enumerations :: [Enumeration]
  , typeAliases :: [TypeAlias]
  }
  deriving stock (Show, Eq, Ord, Lift)

-- We need to do some massaging to make sure that we get the right aeson instances for
-- these types and can actually parse the incoming data!
$( let
    -- "type" is a very common field name, we use "type_" on the Haskell side
    defOpts = defaultOptions{fieldLabelModifier = \case "type_" -> "type"; x -> x}

    propertyInst = JSON.deriveJSON defOpts ''Property
    slInst = JSON.deriveJSON defOpts ''StructureLiteral

    -- 'BaseType' is a union of strings, so we encode it as an untagged sum with some
    -- mangling of the constructor names
    baseTyNameToTag :: String -> String
    baseTyNameToTag = \case
      "Integer" -> "integer"
      "UInteger" -> "uinteger"
      "Decimal" -> "decimal"
      "String" -> "string"
      "Boolean" -> "boolean"
      "Null" -> "null"
      x -> x
    baseTyNameInst = JSON.deriveJSON (defOpts{sumEncoding = JSON.UntaggedValue, constructorTagModifier = baseTyNameToTag}) ''BaseTypeName

    -- 'Type' is a *tagged* union, but the tag is a string field (sigh), fortunately
    -- aeson can deal with this. Also needs some constructor mangling.
    typeToTag :: String -> String
    typeToTag = \case
      "BaseType" -> "base"
      "ReferenceType" -> "reference"
      "ArrayType" -> "array"
      "MapType" -> "map"
      "AndType" -> "and"
      "OrType" -> "or"
      "TupleType" -> "tuple"
      "StructureLiteralType" -> "literal"
      "StringLiteralType" -> "stringLiteral"
      "IntegerLiteralType" -> "integerLiteral"
      "BooleanLiteralType" -> "booleanLiteral"
      x -> x
    typeOpts =
      defOpts
        { sumEncoding = JSON.defaultTaggedObject{tagFieldName = "kind"}
        , constructorTagModifier = typeToTag
        , fieldLabelModifier = \s -> over _head Char.toLower $ Prelude.dropWhile Char.isLower s
        }
    typeInst = JSON.deriveJSON typeOpts ''Type

    -- The rest are mostly normal
    reqInst = JSON.deriveJSON defOpts ''Request
    notInst = JSON.deriveJSON defOpts ''Notification
    sInst = JSON.deriveJSON defOpts ''Structure
    taInst = JSON.deriveJSON defOpts ''TypeAlias
    -- TextOrInteger is also an untagged sum
    tiInst = JSON.deriveJSON (defOpts{sumEncoding = UntaggedValue}) ''TextOrInteger
    eeInst = JSON.deriveJSON defOpts ''EnumerationEntry
    eInst = JSON.deriveJSON defOpts ''Enumeration
    mdInst = JSON.deriveJSON defOpts ''MetaData
    mmInst = JSON.deriveJSON defOpts ''MetaModel
    in
    mconcat <$> sequence [propertyInst, slInst, baseTyNameInst, typeInst, reqInst, notInst, sInst, taInst, tiInst, eeInst, eInst, mdInst, mmInst]
 )

loadMetaModelFromFile :: FilePath -> Q Exp
loadMetaModelFromFile fp = do
  addDependentFile fp
  res <- liftIO $ JSON.eitherDecodeFileStrict' fp
  case res of
    Left e -> fail e
    Right (mm :: MetaModel) -> lift mm
