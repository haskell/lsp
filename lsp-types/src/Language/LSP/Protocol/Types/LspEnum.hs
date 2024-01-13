{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Language.LSP.Protocol.Types.LspEnum where

import Data.Aeson qualified as Aeson
import Data.Kind
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Text qualified as Text

{- | A class for types that represent a LSP enum type.

 This class carries conversion functions to and from the 'base type' of the enum.
 Not all base type values may have corresponding enum values.
-}
class LspEnum a where
  -- | The base type of the enum.
  type EnumBaseType a :: Type

  -- | The known values of this type, the ones listed in the LSP specification.
  knownValues :: Set.Set a
  knownValues = Set.empty

  -- | Convert an enum value to the base type.
  toEnumBaseType :: a -> EnumBaseType a

  -- | Convert a base type value to an enum value, failing if it does not correspond to
  -- an enum value.
  fromEnumBaseType :: EnumBaseType a -> Maybe a
  default fromEnumBaseType :: (LspOpenEnum a) => EnumBaseType a -> Maybe a
  fromEnumBaseType = Just . fromOpenEnumBaseType

{- | A class for types that represent a LSP open enum type.

 Open enum types allow any base type value to be used as a 'custom' enum value.
-}
class LspEnum a => LspOpenEnum a where
  -- | Convert a base type to an enum value. All base type values can be converted this way.
  fromOpenEnumBaseType :: EnumBaseType a -> a

{- | Newtype for @deriving via@ to get standard JSON and 'IsString' instances in terms of the 'LspEnum'
 class methods.
-}
newtype AsLspEnum a = AsLspEnum a

instance (LspEnum a, EnumBaseType a ~ b, Aeson.ToJSON b) => Aeson.ToJSON (AsLspEnum a) where
  toJSON (AsLspEnum e) = Aeson.toJSON (toEnumBaseType e)

instance (LspEnum a, EnumBaseType a ~ b, Aeson.FromJSON b, Show b) => Aeson.FromJSON (AsLspEnum a) where
  parseJSON val = do
    v <- Aeson.parseJSON val
    case fromEnumBaseType v of
      Just x -> pure $ AsLspEnum x
      Nothing -> fail $ "unrecognized enum value " ++ show v

instance (LspOpenEnum a, EnumBaseType a ~ b, b ~ Text.Text) => IsString (AsLspEnum a) where
  fromString s = AsLspEnum $ fromOpenEnumBaseType (Text.pack s)
