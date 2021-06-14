{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

module Rel8.Type.Array
  ( array, encodeArrayElement
  , listTypeInformation
  , nonEmptyTypeInformation
  )
where

-- base
import Data.Foldable ( toList )
import Data.List.NonEmpty ( NonEmpty, nonEmpty )
import Prelude hiding ( null, repeat, zipWith )

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Schema.Null ( Unnullify, Nullity( Null, NotNull ) )
import Rel8.Type.Information ( TypeInformation(..), parseTypeInformation )


array :: Foldable f
  => TypeInformation a -> f Opaleye.PrimExpr -> Opaleye.PrimExpr
array info =
  Opaleye.CastExpr (arrayType info <> "[]") .
  Opaleye.ArrayExpr . map (encodeArrayElement info) . toList
{-# INLINABLE array #-}


listTypeInformation :: ()
  => Nullity a
  -> TypeInformation (Unnullify a)
  -> TypeInformation [a]
listTypeInformation nullity info@TypeInformation {encode, decode} =
  TypeInformation
    { decode = case nullity of
        Null ->
          Hasql.listArray (decodeArrayElement info (Hasql.nullable decode))
        NotNull ->
          Hasql.listArray (decodeArrayElement info (Hasql.nonNullable decode))
    , encode = case nullity of
        Null ->
          Opaleye.ArrayExpr .
          fmap (encodeArrayElement info . maybe null encode)
        NotNull ->
          Opaleye.ArrayExpr .
          fmap (encodeArrayElement info . encode)
    , typeName = arrayType info <> "[]"
    }
  where
    null = Opaleye.ConstExpr Opaleye.NullLit


nonEmptyTypeInformation :: ()
  => Nullity a
  -> TypeInformation (Unnullify a)
  -> TypeInformation (NonEmpty a)
nonEmptyTypeInformation nullity =
  parseTypeInformation parse toList . listTypeInformation nullity
  where
    parse = maybe (Left message) Right . nonEmpty
    message = "failed to decode NonEmptyList: got empty list"


isArray :: TypeInformation a -> Bool
isArray = \case
  (reverse . typeName -> ']' : '[' : _) -> True
  _ -> False


arrayType :: TypeInformation a -> String
arrayType info
  | isArray info = "record"
  | otherwise = typeName info


decodeArrayElement :: TypeInformation a -> Hasql.NullableOrNot Hasql.Value x -> Hasql.NullableOrNot Hasql.Value x
decodeArrayElement info
  | isArray info = Hasql.nonNullable . Hasql.composite . Hasql.field
  | otherwise = id


encodeArrayElement :: TypeInformation a -> Opaleye.PrimExpr -> Opaleye.PrimExpr
encodeArrayElement info
  | isArray info = Opaleye.UnExpr (Opaleye.UnOpOther "ROW")
  | otherwise = id
