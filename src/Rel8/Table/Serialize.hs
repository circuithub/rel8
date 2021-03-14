{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Serialize
  ( Serializable, lit, parse
  )
where

-- base
import Data.Bifunctor ( bimap )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Serialize ( slitExpr, sparseValue )
import Rel8.Kind.Blueprint
  ( Blueprint( Scalar )
  , KnownBlueprint
  , FromDBType, ToDBType
  , FromType, ToType
  )
import Rel8.Kind.Emptiability ( Emptiability( Emptiable, NonEmptiable ) )
import Rel8.Kind.Nullability
  ( Nullability( Nullable, NonNullable )
  , KnownNullability
  )
import Rel8.Schema.Context ( DB(..), Result(..) )
import Rel8.Schema.Context.Result
  ( fromHEitherTable, toHEitherTable
  , fromHListTable, toHListTable
  , fromHMaybeTable, toHMaybeTable
  , fromHNonEmptyTable, toHNonEmptyTable
  , fromHTheseTable, toHTheseTable
  )
import Rel8.Schema.HTable ( HTable, htabulate, htabulateA, hfield, hspecs )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import Rel8.Schema.HTable.Quartet ( HQuartet(..) )
import Rel8.Schema.HTable.Quintet ( HQuintet(..) )
import Rel8.Schema.HTable.Pair ( HPair(..) )
import Rel8.Schema.HTable.Trio ( HTrio(..) )
import Rel8.Schema.HTable.Context ( H )
import Rel8.Schema.Recontextualize ( Recontextualize )
import Rel8.Schema.Spec ( SSpec( SSpec ), KnownSpec )
import Rel8.Schema.Value
  ( Value( NullableValue, NonNullableValue )
  , FromValue, ToValue
  )
import Rel8.Table ( Table, Columns, Context, fromColumns, toColumns )
import Rel8.Table.Either ( EitherTable )
import Rel8.Table.List ( ListTable )
import Rel8.Table.Maybe ( MaybeTable )
import Rel8.Table.NonEmpty ( NonEmptyTable )
import Rel8.Table.These ( TheseTable )
import Rel8.Type ( DBType )
import Rel8.Type.Array ( Array )

-- these
import Data.These ( These )


type IsTabular :: Type -> Bool
type family IsTabular a where
  IsTabular (Either _ _) = 'True
  IsTabular [a] = IsListTabular a
  IsTabular (Maybe a) = IsMaybeTabular a
  IsTabular (NonEmpty a) = IsListTabular a
  IsTabular (These _ _) = 'True
  IsTabular (_, _) = 'True
  IsTabular (_, _, _) = 'True
  IsTabular (_, _, _, _) = 'True
  IsTabular (_, _, _, _, _) = 'True
  IsTabular (_ (H Result)) = 'True
  IsTabular (_ Result) = 'True
  IsTabular (Result _) = 'True
  IsTabular _ = 'False


type IsMaybeTabular :: Type -> Bool
type family IsMaybeTabular a where
  IsMaybeTabular (Maybe _) = 'True
  IsMaybeTabular a = IsTabular a


type IsListTabular :: Type -> Bool
type family IsListTabular a where
  IsListTabular (Maybe a) = IsMaybeTabular a
  IsListTabular a = IsTabular a


type ToExprs :: Type -> Type -> Constraint
class ExprsFor (IsTabular a) a exprs => ToExprs a exprs
instance ExprsFor (IsTabular a) a exprs => ToExprs a exprs


type ExprsFor :: Bool -> Type -> Type -> Constraint
class (Table exprs, Context exprs ~ DB, isTabular ~ IsTabular a) => ExprsFor isTabular a exprs where
  fromResults :: Columns exprs (H Result) -> a
  toResults :: a -> Columns exprs (H Result)


instance
  ( exprs ~ Expr 'NonNullable a
  , FromDBType a ~ 'Scalar a
  , IsTabular a ~ 'False
  , DBType a
  ) => ExprsFor 'False a exprs
 where
  fromResults (HIdentity (Result (NonNullableValue a))) = a
  toResults = HIdentity . Result . NonNullableValue


instance
  ( '(nullability, a) ~ ToValue ma
  , FromValue nullability a ~ ma
  , blueprint ~ FromDBType dbType
  , blueprint ~ FromType a
  , ToDBType blueprint ~ dbType
  , ToType blueprint ~ a
  , KnownBlueprint blueprint
  , KnownNullability nullability
  , DBType dbType
  , IsListTabular ma ~ 'False
  , x ~ Array 'Emptiable nullability dbType
  , outerNullability ~ 'NonNullable
  ) => ExprsFor 'False [ma] (Expr outerNullability x)
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
  , IsMaybeTabular a ~ 'False
  , x ~ Maybe a
  ) => ExprsFor 'False (Maybe a) (Expr nullability dbType)
 where
  fromResults (HIdentity (Result (NullableValue a))) = a
  toResults = HIdentity . Result . NullableValue


instance
  ( '(nullability, a) ~ ToValue ma
  , FromValue nullability a ~ ma
  , blueprint ~ FromDBType dbType
  , blueprint ~ FromType a
  , ToDBType blueprint ~ dbType
  , ToType blueprint ~ a
  , KnownBlueprint blueprint
  , KnownNullability nullability
  , DBType dbType
  , IsListTabular ma ~ 'False
  , x ~ Array 'NonEmptiable nullability dbType
  , outerNullability ~ 'NonNullable
  ) => ExprsFor 'False (NonEmpty ma) (Expr outerNullability x)
 where
  fromResults (HIdentity (Result (NonNullableValue a))) = a
  toResults = HIdentity . Result . NonNullableValue


instance
  ( ToExprs a exprs
  , IsListTabular a ~ 'False
  ) => ExprsFor 'False [a] (ListTable exprs)
 where
  fromResults = fmap (fromResults @_ @_ @exprs) . fromHListTable
  toResults = toHListTable . fmap (toResults @_ @_ @exprs)


instance
  ( ToExprs a exprs
  , IsMaybeTabular a ~ 'False
  ) => ExprsFor 'False (Maybe a) (MaybeTable exprs)
 where
  fromResults = fmap (fromResults @_ @_ @exprs) . fromHMaybeTable
  toResults = toHMaybeTable . fmap (toResults @_ @_ @exprs)


instance
  ( ToExprs a exprs
  , IsListTabular a ~ 'False
  ) => ExprsFor 'False (NonEmpty a) (NonEmptyTable exprs)
 where
  fromResults = fmap (fromResults @_ @_ @exprs) . fromHNonEmptyTable
  toResults = toHNonEmptyTable . fmap (toResults @_ @_ @exprs)


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , isTabular ~ 'True
  , x ~ EitherTable exprs1 exprs2
  ) => ExprsFor isTabular (Either a b) x
 where
  fromResults = bimap (fromResults @_ @_ @exprs1) (fromResults @_ @_ @exprs2) . fromHEitherTable
  toResults = toHEitherTable . bimap (toResults @_ @_ @exprs1) (toResults @_ @_ @exprs2)


instance
  ( ToExprs a exprs
  , IsListTabular a ~ 'True
  , x ~ ListTable exprs
  ) => ExprsFor 'True [a] x
 where
  fromResults = fmap (fromResults @_ @_ @exprs) . fromHListTable
  toResults = toHListTable . fmap (toResults @_ @_ @exprs)


instance
  ( ToExprs a exprs
  , IsMaybeTabular a ~ 'True
  , x ~ MaybeTable exprs
  ) => ExprsFor 'True (Maybe a) x
 where
  fromResults = fmap (fromResults @_ @_ @exprs) . fromHMaybeTable
  toResults = toHMaybeTable . fmap (toResults @_ @_ @exprs)


instance
  ( ToExprs a exprs
  , IsListTabular a ~ 'True
  , x ~ NonEmptyTable exprs
  ) => ExprsFor 'True (NonEmpty a) x
 where
  fromResults = fmap (fromResults @_ @_ @exprs) . fromHNonEmptyTable
  toResults = toHNonEmptyTable . fmap (toResults @_ @_ @exprs)


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , isTabular ~ 'True
  , x ~ TheseTable exprs1 exprs2
  ) => ExprsFor isTabular (These a b) x
 where
  fromResults = bimap (fromResults @_ @_ @exprs1) (fromResults @_ @_ @exprs2) . fromHTheseTable
  toResults = toHTheseTable . bimap (toResults @_ @_ @exprs1) (toResults @_ @_ @exprs2)


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , isTabular ~ 'True
  , x ~ (exprs1, exprs2)
  ) => ExprsFor isTabular (a, b) x
 where
  fromResults (HPair a b) =
    ( fromResults @_ @_ @exprs1 a
    , fromResults @_ @_ @exprs2 b
    )
  toResults (a, b) = HPair
    { hfst = toResults @_ @_ @exprs1 a
    , hsnd = toResults @_ @_ @exprs2 b
    }


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , ToExprs c exprs3
  , isTabular ~ 'True
  , x ~ (exprs1, exprs2, exprs3)
  ) => ExprsFor isTabular (a, b, c) x
 where
  fromResults (HTrio a b c) =
    ( fromResults @_ @_ @exprs1 a
    , fromResults @_ @_ @exprs2 b
    , fromResults @_ @_ @exprs3 c
    )
  toResults (a, b, c) = HTrio
    { hfst = toResults @_ @_ @exprs1 a
    , hsnd = toResults @_ @_ @exprs2 b
    , htrd = toResults @_ @_ @exprs3 c
    }


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , ToExprs c exprs3
  , ToExprs d exprs4
  , isTabular ~ 'True
  , x ~ (exprs1, exprs2, exprs3, exprs4)
  ) => ExprsFor isTabular (a, b, c, d) x
 where
  fromResults (HQuartet a b c d) =
    ( fromResults @_ @_ @exprs1 a
    , fromResults @_ @_ @exprs2 b
    , fromResults @_ @_ @exprs3 c
    , fromResults @_ @_ @exprs4 d
    )
  toResults (a, b, c, d) = HQuartet
    { hfst = toResults @_ @_ @exprs1 a
    , hsnd = toResults @_ @_ @exprs2 b
    , htrd = toResults @_ @_ @exprs3 c
    , hfrt = toResults @_ @_ @exprs4 d
    }


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , ToExprs c exprs3
  , ToExprs d exprs4
  , ToExprs e exprs5
  , isTabular ~ 'True
  , x ~ (exprs1, exprs2, exprs3, exprs4, exprs5)
  ) => ExprsFor isTabular (a, b, c, d, e) x
 where
  fromResults (HQuintet a b c d e) =
    ( fromResults @_ @_ @exprs1 a
    , fromResults @_ @_ @exprs2 b
    , fromResults @_ @_ @exprs3 c
    , fromResults @_ @_ @exprs4 d
    , fromResults @_ @_ @exprs5 e
    )
  toResults (a, b, c, d, e) = HQuintet
    { hfst = toResults @_ @_ @exprs1 a
    , hsnd = toResults @_ @_ @exprs2 b
    , htrd = toResults @_ @_ @exprs3 c
    , hfrt = toResults @_ @_ @exprs4 d
    , hfft = toResults @_ @_ @exprs5 e
    }


instance
  ( HTable t
  , isTabular ~ 'True
  , result ~ H Result
  , x ~ t (H DB)
  ) => ExprsFor isTabular (t result) x
 where
  fromResults = id
  toResults = id


instance
  ( Recontextualize DB Result (t DB) (t Result)
  , isTabular ~ 'True
  , result ~ Result
  , x ~ t DB
  ) => ExprsFor isTabular (t result) x
 where
  fromResults = fromColumns
  toResults = toColumns


instance
  ( KnownSpec spec
  , isTabular ~ 'True
  , x ~ DB spec
  ) => ExprsFor isTabular (Result spec) x
 where
  fromResults = fromColumns
  toResults = toColumns


type FromExprs :: Type -> Type
type family FromExprs a where
  FromExprs (Expr nullability a) =
    FromValue nullability (ToType (FromDBType a))
  FromExprs (DB spec) = Result spec
  FromExprs (EitherTable a b) = Either (FromExprs a) (FromExprs b)
  FromExprs (ListTable a) = [FromExprs a]
  FromExprs (MaybeTable a) = Maybe (FromExprs a)
  FromExprs (NonEmptyTable a) = NonEmpty (FromExprs a)
  FromExprs (TheseTable a b) = These (FromExprs a) (FromExprs b)
  FromExprs (a, b) = (FromExprs a, FromExprs b)
  FromExprs (a, b, c) = (FromExprs a, FromExprs b, FromExprs c)
  FromExprs (a, b, c, d) =
    (FromExprs a, FromExprs b, FromExprs c, FromExprs d)
  FromExprs (a, b, c, d, e) =
    (FromExprs a, FromExprs b, FromExprs c, FromExprs d, FromExprs e)
  FromExprs (t DB) = t Result
  FromExprs (t (H DB)) = t (H Result)


class (ToExprs a exprs, a ~ FromExprs exprs) => Serializable exprs a | exprs -> a
instance (ToExprs a exprs, a ~ FromExprs exprs) => Serializable exprs a


lit :: forall exprs a. Serializable exprs a => a -> exprs
lit = fromColumns . litTable . toResults @_ @_ @exprs


parse :: forall exprs a. Serializable exprs a => Hasql.Row a
parse = fromResults @_ @_ @exprs <$> parseTable


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
