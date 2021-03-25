{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

module Rel8.Type.Array
  ( array
  , listTypeInformation
  , nonEmptyTypeInformation
  , fromPrimArray, toPrimArray, zipPrimArraysWith
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
import Rel8.Schema.Nullability ( Unnullify, Nullability( Nullable, NonNullable ) )
import Rel8.Type.Information ( TypeInformation(..), parseTypeInformation )


array :: Foldable f
  => TypeInformation a -> f Opaleye.PrimExpr -> Opaleye.PrimExpr
array TypeInformation {typeName} =
  fromPrimArray .
  Opaleye.CastExpr (typeName <> "[]") .
  Opaleye.ArrayExpr . toList


listTypeInformation :: ()
  => Nullability a
  -> TypeInformation (Unnullify a)
  -> TypeInformation [a]
listTypeInformation nullability info = 
  case info of
    TypeInformation{ encode, decode, out } -> TypeInformation
      { decode = row $ case nullability of
          Nullable -> Hasql.listArray (Hasql.nullable (out <$> decode))
          NonNullable -> Hasql.listArray (Hasql.nonNullable (out <$> decode))
      , encode = case nullability of
          Nullable -> array info . fmap (maybe null encode)
          NonNullable -> array info . fmap encode
      , typeName = "record"
      , out = id
      }
  where
    row = Hasql.composite . Hasql.field . Hasql.nonNullable
    null = Opaleye.ConstExpr Opaleye.NullLit


nonEmptyTypeInformation :: ()
  => Nullability a
  -> TypeInformation (Unnullify a)
  -> TypeInformation (NonEmpty a)
nonEmptyTypeInformation nullability =
  parseTypeInformation parse toList . listTypeInformation nullability
  where
    parse = maybe (Left message) Right . nonEmpty
    message = "failed to decode NonEmptyList: got empty list"


fromPrimArray :: Opaleye.PrimExpr -> Opaleye.PrimExpr
fromPrimArray = Opaleye.UnExpr (Opaleye.UnOpOther "ROW")


toPrimArray :: Opaleye.PrimExpr -> Opaleye.PrimExpr
toPrimArray a = Opaleye.CompositeExpr a "f1"


zipPrimArraysWith :: ()
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr -> Opaleye.PrimExpr)
  -> Opaleye.PrimExpr -> Opaleye.PrimExpr -> Opaleye.PrimExpr
zipPrimArraysWith f a b = fromPrimArray (f (toPrimArray a) (toPrimArray b))
