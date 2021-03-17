{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Type.Array
  ( Array(..), array
  , arrayTypeInformation
  , (++.), sempty
  )
where

-- base
import Data.Foldable ( toList )
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty, nonEmpty )
import Prelude hiding ( null, repeat, zipWith )

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Kind.Emptiability
  ( Emptiability( Emptiable, NonEmptiable )
  , SEmptiability( SEmptiable, SNonEmptiable )
  , KnownEmptiability
  , emptiabilitySing
  )
import Rel8.Kind.Nullability
  ( Nullability( Nullable, NonNullable )
  , SNullability( SNullable, SNonNullable )
  , KnownNullability
  , nullabilitySing
  )
import Rel8.Type ( DBType, typeInformation, TypeInformation(..) )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Monoid ( DBMonoid, memptyExpr )
import Rel8.Type.Ord ( DBOrd )
import Rel8.Type.Semigroup ( DBSemigroup, (<>.) )


type Array :: Emptiability -> Nullability -> Type -> Type
data Array emptiability nullability a where
  NullableList :: [Maybe a] -> Array 'Emptiable 'Nullable a
  NullableNonEmpty :: NonEmpty (Maybe a) -> Array 'NonEmptiable 'Nullable a
  NonNullableList :: [a] -> Array 'Emptiable 'NonNullable a
  NonNullableNonEmpty :: NonEmpty a -> Array 'NonEmptiable 'NonNullable a


instance
  ( KnownEmptiability emptiability
  , KnownNullability nullability
  , DBType a
  ) => DBType (Array emptiability nullability a)
 where
  typeInformation =
    arrayTypeInformation emptiabilitySing nullabilitySing typeInformation


instance
  ( KnownEmptiability emptiability
  , KnownNullability nullability
  , DBEq a
  ) => DBEq (Array emptiability nullability a)


instance
  ( KnownEmptiability emptiability
  , KnownNullability nullability
  , DBOrd a
  ) => DBOrd (Array emptiability nullability a)


instance
  ( KnownEmptiability emptiability
  , KnownNullability nullability
  , DBType a
  ) => DBSemigroup (Array emptiability nullability a)
 where
  (<>.) = (++.)


instance
  ( KnownEmptiability emptiability
  , KnownNullability nullability
  , DBType a
  ) => DBMonoid (Array emptiability nullability a)
 where
  memptyExpr = Expr (array (typeInformation @a) [])


array :: Foldable f
  => TypeInformation a -> f Opaleye.PrimExpr -> Opaleye.PrimExpr
array TypeInformation {typeName} =
  Opaleye.UnExpr (Opaleye.UnOpOther "ROW") .
  Opaleye.CastExpr (typeName <> "[]") .
  Opaleye.ArrayExpr . toList


arrayTypeInformation :: ()
  => SEmptiability emptiability
  -> SNullability nullability
  -> TypeInformation a
  -> TypeInformation (Array emptiability nullability a)
arrayTypeInformation emptiability nullability info = TypeInformation
  { decode = row $ case (emptiability, nullability) of
      (SEmptiable, SNullable) -> NullableList <$>
        Hasql.listArray (Hasql.nullable decode)
      (SNonEmptiable, SNullable) -> NullableNonEmpty <$>
        nonEmptyArray (Hasql.nullable decode)
      (SEmptiable, SNonNullable) -> NonNullableList <$>
        Hasql.listArray (Hasql.nonNullable decode)
      (SNonEmptiable, SNonNullable) -> NonNullableNonEmpty <$>
        nonEmptyArray (Hasql.nonNullable decode)
  , encode = \case
      NullableList as -> array info (maybe null encode <$> as)
      NonNullableList as -> array info (encode <$> as)
      NullableNonEmpty as -> array info (maybe null encode <$> as)
      NonNullableNonEmpty as -> array info (encode <$> as)
  , typeName = "record"
  }
  where
    TypeInformation {encode, decode} = info
    row = Hasql.composite . Hasql.field . Hasql.nonNullable
    nonEmptyArray =
      Hasql.refine (maybe (Left message) Right . nonEmpty) . Hasql.listArray
      where
        message = "failed to decode NonEmptiable Array: empty list"
    null = Opaleye.ConstExpr Opaleye.NullLit


(++.) :: ()
  => Expr 'NonNullable (Array emptiability nullability a)
  -> Expr 'NonNullable (Array emptiability nullability a)
  -> Expr 'NonNullable (Array emptiability nullability a)
Expr a ++. Expr b = Expr $
  Opaleye.UnExpr (Opaleye.UnOpOther "ROW") $
  Opaleye.BinExpr (Opaleye.:||) (unrow a) (unrow b)
infixr 5 ++.


sempty :: ()
  => TypeInformation a -> Expr 'NonNullable (Array 'Emptiable nullability a)
sempty info = Expr (array info [])


-- Requires Postgres 13
unrow :: Opaleye.PrimExpr -> Opaleye.PrimExpr
unrow a = Opaleye.CompositeExpr a "f1"
