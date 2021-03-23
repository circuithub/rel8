{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

module Rel8.Type.Array
  ( array
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
import Rel8.Schema.Nullability ( Nullability( Nullable, NonNullable ) )
import Rel8.Type.Information ( TypeInformation(..), parseTypeInformation )


array :: Foldable f
  => TypeInformation a -> f Opaleye.PrimExpr -> Opaleye.PrimExpr
array TypeInformation {typeName} =
  Opaleye.UnExpr (Opaleye.UnOpOther "ROW") .
  Opaleye.CastExpr (typeName <> "[]") .
  Opaleye.ArrayExpr . toList


listTypeInformation :: ()
  => Nullability a ma
  -> TypeInformation a
  -> TypeInformation [ma]
listTypeInformation nullability info = TypeInformation
  { decode = row $ case nullability of
      Nullable -> Hasql.listArray (Hasql.nullable decode)
      NonNullable -> Hasql.listArray (Hasql.nonNullable decode)
  , encode = case nullability of
      Nullable -> array info . fmap (maybe null encode)
      NonNullable -> array info . fmap encode
  , typeName = "record"
  }
  where
    TypeInformation {encode, decode} = info
    row = Hasql.composite . Hasql.field . Hasql.nonNullable
    null = Opaleye.ConstExpr Opaleye.NullLit


nonEmptyTypeInformation :: ()
  => Nullability a ma
  -> TypeInformation a
  -> TypeInformation (NonEmpty ma)
nonEmptyTypeInformation nullability =
  parseTypeInformation parse toList . listTypeInformation nullability
  where
    parse = maybe (Left message) Right . nonEmpty
    message = "failed to decode NonEmptyList: got empty list"
