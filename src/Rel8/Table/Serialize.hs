{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Serialize
  ( Serializable( fromResults, toResults )
  , lit, parse
  , litTable, parseTable
  )
where

-- base
import Data.Bifunctor ( bimap )
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import Rel8.Expr
import Rel8.Expr.Serialize ( slitExpr, sparseValue )
import Rel8.Kind.Blueprint
import Rel8.Kind.Emptiability
import Rel8.Kind.Nullability
import Rel8.Schema.Context ( DB(..), Result(..) )
import Rel8.Schema.Context.Result
import Rel8.Schema.Generic
import Rel8.Schema.HTable ( HTable, htabulate, htabulateA, hfield, hspecs )
import Rel8.Schema.HTable.Identity
import Rel8.Schema.HTable.Pair
import Rel8.Schema.HTable.Trio
import Rel8.Schema.HTable.Context ( H )
import Rel8.Schema.Recontextualize ( Recontextualize )
import Rel8.Schema.Spec ( SSpec( SSpec ) )
import Rel8.Schema.Value
import Rel8.Table ( Table, Columns, Context, fromColumns, toColumns )
import Rel8.Table.Either
import Rel8.Table.List
import Rel8.Table.Maybe
import Rel8.Table.NonEmpty
import Rel8.Table.These
import Rel8.Type
import Rel8.Type.Array

-- these
import Data.These ( These )


class (Table exprs, Context exprs ~ DB) => Serializable exprs a where
  fromResults :: Columns exprs (H Result) -> a
  toResults :: a -> Columns exprs (H Result)


instance {-# OVERLAPPABLE #-}
  ( exprs ~ Expr 'NonNullable a
  , 'Scalar a ~ FromDBType a
  , DBType a
  ) => Serializable exprs a
 where
  fromResults (HIdentity (Result (NonNullableValue a))) = a
  toResults = HIdentity . Result . NonNullableValue


instance
  ( nullability ~ 'Nullable
  , blueprint ~ FromDBType dbType
  , blueprint ~ FromType a
  , ToDBType blueprint ~ dbType
  , ToType blueprint ~ a
  , KnownBlueprint blueprint
  , DBType dbType
  ) => Serializable (Expr nullability dbType) (Maybe a)
 where
  fromResults (HIdentity (Result (NullableValue a))) = a
  toResults = HIdentity . Result . NullableValue


instance
  ( outerNullability ~ 'NonNullable
  , array ~ Array emptiability nullability dbType
  , emptiability ~ 'Emptiable
  , '(nullability, a) ~ ToValue x
  , x ~ FromValue nullability a
  , blueprint ~ FromDBType dbType
  , blueprint ~ FromType a
  , ToDBType blueprint ~ dbType
  , ToType blueprint ~ a
  , KnownBlueprint blueprint
  , KnownNullability nullability
  , DBType dbType
  ) => Serializable (Expr outerNullability array) [x]
 where
  fromResults (HIdentity (Result (NonNullableValue a))) = a
  toResults = HIdentity . Result . NonNullableValue


instance
  ( outerNullability ~ 'NonNullable
  , array ~ Array emptiability nullability dbType
  , emptiability ~ 'NonEmptiable
  , '(nullability, a) ~ ToValue x
  , x ~ FromValue nullability a
  , blueprint ~ FromDBType dbType
  , blueprint ~ FromType a
  , ToDBType blueprint ~ dbType
  , ToType blueprint ~ a
  , KnownBlueprint blueprint
  , KnownNullability nullability
  , DBType dbType
  ) => Serializable (Expr outerNullability array) (NonEmpty x)
 where
  fromResults (HIdentity (Result (NonNullableValue a))) = a
  toResults = HIdentity . Result . NonNullableValue


instance
  ( either ~ EitherTable exprs1 exprs2
  , Serializable exprs1 a
  , Serializable exprs2 b
  ) => Serializable either (Either a b)
 where
  fromResults = bimap (fromResults @exprs1) (fromResults @exprs2) . fromHEitherTable
  toResults = toHEitherTable . bimap (toResults @exprs1) (toResults @exprs2)


instance Serializable exprs a => Serializable (ListTable exprs) [a]
 where
  fromResults = fmap (fromResults @exprs) . fromHListTable
  toResults = toHListTable . fmap (toResults @exprs)


instance Serializable exprs a => Serializable (MaybeTable exprs) (Maybe a)
 where
  fromResults = fmap (fromResults @exprs) . fromHMaybeTable
  toResults = toHMaybeTable . fmap (toResults @exprs)


instance Serializable exprs a =>
  Serializable (NonEmptyTable exprs) (NonEmpty a)
 where
  fromResults = fmap (fromResults @exprs) . fromHNonEmptyTable
  toResults = toHNonEmptyTable . fmap (toResults @exprs)


instance
  ( these ~ TheseTable exprs1 exprs2
  , Serializable exprs1 a
  , Serializable exprs2 b
  ) => Serializable these (These a b)
 where
  fromResults = bimap (fromResults @exprs1) (fromResults @exprs2) . fromHTheseTable
  toResults = toHTheseTable . bimap (toResults @exprs1) (toResults @exprs2)


instance
  ( pair ~ (exprs1, exprs2)
  , Serializable exprs1 a
  , Serializable exprs2 b
  ) => Serializable pair (a, b)
 where
  fromResults (HPair a b) = (fromResults @exprs1 a, fromResults @exprs2 b)
  toResults (a, b) = HPair (toResults @exprs1 a) (toResults @exprs2 b)


instance
  ( trio ~ (exprs1, exprs2, exprs3)
  , Serializable exprs1 a
  , Serializable exprs2 b
  , Serializable exprs3 c
  ) => Serializable trio (a, b, c)
 where
  fromResults (HTrio a b c) =
    ( fromResults @exprs1 a
    , fromResults @exprs2 b
    , fromResults @exprs3 c
    )
  toResults (a, b, c) = HTrio
    { hfst = toResults @exprs1 a
    , hsnd = toResults @exprs2 b
    , htrd = toResults @exprs3 c
    }


instance (HTable t, exprs ~ t (H DB), result ~ H Result) =>
  Serializable exprs (t result)
 where
  fromResults = id
  toResults = id


instance (Rel8able t, exprs ~ t DB, result ~ Result) =>
  Serializable exprs (t result)
 where
  fromResults = fromColumns
  toResults = toColumns


lit :: forall exprs a. Serializable exprs a => a -> exprs
lit = fromColumns . litTable . toResults @exprs


parse :: forall exprs a. Serializable exprs a => Hasql.Row a
parse = fromResults @exprs <$> parseTable


litTable :: Recontextualize Result DB a b => a -> b
litTable (toColumns -> as) = fromColumns $ htabulate $ \field ->
  case hfield hspecs field of
    SSpec _ _ blueprint info -> case hfield as field of
      Result value -> DB (slitExpr blueprint info value)


parseTable :: (Table a, Context a ~ Result) => Hasql.Row a
parseTable = fmap fromColumns $ htabulateA $ \field ->
  case hfield hspecs field of
    SSpec _ nullability blueprint info ->
      Result <$> sparseValue nullability blueprint info
