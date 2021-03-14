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
  ( ToExprs, fromResults, toResults
  , FromExprs
  , Serializable
  , lit, parse
  , litTable, parseTable
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


data Shape
  = Either Shape Shape
  | List Shape
  | Maybe Shape
  | NonEmpty Shape
  | These Shape Shape
  | Pair Shape Shape
  | Trio Shape Shape Shape
  | Quartet Shape Shape Shape Shape
  | Quintet Shape Shape Shape Shape Shape
  | HTable
  | Rel8able
  | Field
  | Column


type Trace :: Type -> Shape
type family Trace a where
  Trace (Either a b) = 'Either (Trace a) (Trace b)
  Trace [a] = 'List (Trace a)
  Trace (Maybe a) = 'Maybe (Trace a)
  Trace (NonEmpty a) = 'NonEmpty (Trace a)
  Trace (These a b) = 'These (Trace a) (Trace b)
  Trace (a, b) = 'Pair (Trace a) (Trace b)
  Trace (a, b, c) = 'Trio (Trace a) (Trace b) (Trace c)
  Trace (a, b, c, d) = 'Quartet (Trace a) (Trace b) (Trace c) (Trace d)
  Trace (a, b, c, d, e) = 'Quintet (Trace a) (Trace b) (Trace c) (Trace d) (Trace e)
  Trace (_ (H Result)) = 'HTable
  Trace (_ Result) = 'Rel8able
  Trace (Result _) = 'Field
  Trace _ = 'Column


type IsTabular :: Shape -> Bool
type family IsTabular shape where
  IsTabular ('Either _ _) = 'True
  IsTabular ('List a) = IsListTabular a
  IsTabular ('Maybe a) = IsMaybeTabular a
  IsTabular ('NonEmpty a) = IsListTabular a
  IsTabular ('These _ _) = 'True
  IsTabular ('Pair _ _) = 'True
  IsTabular ('Trio _ _ _) = 'True
  IsTabular ('Quartet _ _ _ _) = 'True
  IsTabular ('Quintet _ _ _ _ _) = 'True
  IsTabular 'HTable = 'True
  IsTabular 'Rel8able = 'True
  IsTabular 'Field = 'True
  IsTabular 'Column = 'False


type IsMaybeTabular :: Shape -> Bool
type family IsMaybeTabular shape where
  IsMaybeTabular ('Maybe _) = 'True
  IsMaybeTabular a = IsTabular a


type IsListTabular :: Shape -> Bool
type family IsListTabular shape where
  IsListTabular ('Maybe a) = IsMaybeTabular a
  IsListTabular a = IsTabular a


type ToExprs :: Type -> Type -> Constraint
class ExprsFor (Trace a) (IsTabular (Trace a)) a exprs => ToExprs a exprs
instance ExprsFor (Trace a) (IsTabular (Trace a)) a exprs => ToExprs a exprs


type ExprsFor :: Shape -> Bool -> Type -> Type -> Constraint
class (Table exprs, Context exprs ~ DB, shape ~ Trace a, isTabular ~ IsTabular shape) => ExprsFor shape isTabular a exprs where
  fromResults :: Columns exprs (H Result) -> a
  toResults :: a -> Columns exprs (H Result)


instance
  ( exprs ~ Expr 'NonNullable a
  , FromDBType a ~ 'Scalar a
  , DBType a
  , Trace a ~ 'Column
  ) => ExprsFor 'Column 'False a exprs
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
  , Trace ma ~ shape
  , IsListTabular shape ~ 'False
  , x ~ [ma]
  , y ~ Array 'Emptiable nullability dbType
  , outerNullability ~ 'NonNullable
  ) => ExprsFor ('List shape) 'False x (Expr outerNullability y)
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
  , Trace a ~ shape
  , IsMaybeTabular shape ~ 'False
  , x ~ Maybe a
  ) => ExprsFor ('Maybe shape) 'False x (Expr nullability dbType)
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
  , Trace ma ~ shape
  , IsListTabular shape ~ 'False
  , x ~ NonEmpty ma
  , y ~ Array 'NonEmptiable nullability dbType
  , outerNullability ~ 'NonNullable
  ) => ExprsFor ('NonEmpty shape) 'False x (Expr outerNullability y)
 where
  fromResults (HIdentity (Result (NonNullableValue a))) = a
  toResults = HIdentity . Result . NonNullableValue


instance
  ( ExprsFor shape 'False a exprs
  , IsListTabular shape ~ 'False
  , x ~ [a]
  ) => ExprsFor ('List shape) 'False x (ListTable exprs)
 where
  fromResults = fmap (fromResults @_ @_ @_ @exprs) . fromHListTable
  toResults = toHListTable . fmap (toResults @_ @_ @_ @exprs)


instance
  ( ExprsFor shape 'False a exprs
  , IsMaybeTabular shape ~ 'False
  , x ~ Maybe a
  ) => ExprsFor ('Maybe shape) 'False x (MaybeTable exprs)
 where
  fromResults = fmap (fromResults @_ @_ @_ @exprs) . fromHMaybeTable
  toResults = toHMaybeTable . fmap (toResults @_ @_ @_ @exprs)


instance
  ( ExprsFor shape 'False a exprs
  , IsListTabular shape ~ 'False
  , x ~ NonEmpty a
  ) => ExprsFor ('NonEmpty shape) 'False x (NonEmptyTable exprs)
 where
  fromResults = fmap (fromResults @_ @_ @_ @exprs) . fromHNonEmptyTable
  toResults = toHNonEmptyTable . fmap (toResults @_ @_ @_ @exprs)


instance
  ( ExprsFor shape1 (IsTabular shape1) a exprs1
  , ExprsFor shape2 (IsTabular shape2) b exprs2
  , isTabular ~ 'True
  , x ~ Either a b
  , y ~ EitherTable exprs1 exprs2
  ) => ExprsFor ('Either shape1 shape2) isTabular x y
 where
  fromResults = bimap (fromResults @_ @_ @_ @exprs1) (fromResults @_ @_ @_ @exprs2) . fromHEitherTable
  toResults = toHEitherTable . bimap (toResults @_ @_ @_ @exprs1) (toResults @_ @_ @_ @exprs2)


instance
  ( ExprsFor shape (IsTabular shape) a exprs
  , IsListTabular shape ~ 'True
  , x ~ [a]
  , y ~ ListTable exprs
  ) => ExprsFor ('List shape) 'True x y
 where
  fromResults = fmap (fromResults @_ @_ @_ @exprs) . fromHListTable
  toResults = toHListTable . fmap (toResults @_ @_ @_ @exprs)


instance
  ( ExprsFor shape (IsTabular shape) a exprs
  , IsMaybeTabular shape ~ 'True
  , x ~ Maybe a
  , y ~ MaybeTable exprs
  ) => ExprsFor ('Maybe shape) 'True x y
 where
  fromResults = fmap (fromResults @_ @_ @_ @exprs) . fromHMaybeTable
  toResults = toHMaybeTable . fmap (toResults @_ @_ @_ @exprs)


instance
  ( ExprsFor shape (IsTabular shape) a exprs
  , IsListTabular shape ~ 'True
  , x ~ NonEmpty a
  , y ~ NonEmptyTable exprs
  ) => ExprsFor ('NonEmpty shape) 'True x y
 where
  fromResults = fmap (fromResults @_ @_ @_ @exprs) . fromHNonEmptyTable
  toResults = toHNonEmptyTable . fmap (toResults @_ @_ @_ @exprs)


instance
  ( ExprsFor shape1 (IsTabular shape1) a exprs1
  , ExprsFor shape2 (IsTabular shape2) b exprs2
  , isTabular ~ 'True
  , x ~ These a b
  , y ~ TheseTable exprs1 exprs2
  ) => ExprsFor ('These shape1 shape2) isTabular x y
 where
  fromResults = bimap (fromResults @_ @_ @_ @exprs1) (fromResults @_ @_ @_ @exprs2) . fromHTheseTable
  toResults = toHTheseTable . bimap (toResults @_ @_ @_ @exprs1) (toResults @_ @_ @_ @exprs2)


instance
  ( ExprsFor shape1 (IsTabular shape1) a exprs1
  , ExprsFor shape2 (IsTabular shape2) b exprs2
  , isTabular ~ 'True
  , x ~ (a, b)
  , y ~ (exprs1, exprs2)
  ) => ExprsFor ('Pair shape1 shape2) isTabular x y
 where
  fromResults (HPair a b) =
    ( fromResults @_ @_ @_ @exprs1 a
    , fromResults @_ @_ @_ @exprs2 b
    )
  toResults (a, b) = HPair
    { hfst = toResults @_ @_ @_ @exprs1 a
    , hsnd = toResults @_ @_ @_ @exprs2 b
    }


instance
  ( ExprsFor shape1 (IsTabular shape1) a exprs1
  , ExprsFor shape2 (IsTabular shape2) b exprs2
  , ExprsFor shape3 (IsTabular shape3) c exprs3
  , isTabular ~ 'True
  , x ~ (a, b, c)
  , y ~ (exprs1, exprs2, exprs3)
  ) => ExprsFor ('Trio shape1 shape2 shape3) isTabular x y
 where
  fromResults (HTrio a b c) =
    ( fromResults @_ @_ @_ @exprs1 a
    , fromResults @_ @_ @_ @exprs2 b
    , fromResults @_ @_ @_ @exprs3 c
    )
  toResults (a, b, c) = HTrio
    { hfst = toResults @_ @_ @_ @exprs1 a
    , hsnd = toResults @_ @_ @_ @exprs2 b
    , htrd = toResults @_ @_ @_ @exprs3 c
    }


instance
  ( ExprsFor shape1 (IsTabular shape1) a exprs1
  , ExprsFor shape2 (IsTabular shape2) b exprs2
  , ExprsFor shape3 (IsTabular shape3) c exprs3
  , ExprsFor shape4 (IsTabular shape4) d exprs4
  , isTabular ~ 'True
  , x ~ (a, b, c, d)
  , y ~ (exprs1, exprs2, exprs3, exprs4)
  ) => ExprsFor ('Quartet shape1 shape2 shape3 shape4) isTabular x y
 where
  fromResults (HQuartet a b c d) =
    ( fromResults @_ @_ @_ @exprs1 a
    , fromResults @_ @_ @_ @exprs2 b
    , fromResults @_ @_ @_ @exprs3 c
    , fromResults @_ @_ @_ @exprs4 d
    )
  toResults (a, b, c, d) = HQuartet
    { hfst = toResults @_ @_ @_ @exprs1 a
    , hsnd = toResults @_ @_ @_ @exprs2 b
    , htrd = toResults @_ @_ @_ @exprs3 c
    , hfrt = toResults @_ @_ @_ @exprs4 d
    }


instance
  ( ExprsFor shape1 (IsTabular shape1) a exprs1
  , ExprsFor shape2 (IsTabular shape2) b exprs2
  , ExprsFor shape3 (IsTabular shape3) c exprs3
  , ExprsFor shape4 (IsTabular shape4) d exprs4
  , ExprsFor shape5 (IsTabular shape5) e exprs5
  , isTabular ~ 'True
  , x ~ (a, b, c, d, e)
  , y ~ (exprs1, exprs2, exprs3, exprs4, exprs5)
  ) => ExprsFor ('Quintet shape1 shape2 shape3 shape4 shape5) isTabular x y
 where
  fromResults (HQuintet a b c d e) =
    ( fromResults @_ @_ @_ @exprs1 a
    , fromResults @_ @_ @_ @exprs2 b
    , fromResults @_ @_ @_ @exprs3 c
    , fromResults @_ @_ @_ @exprs4 d
    , fromResults @_ @_ @_ @exprs5 e
    )
  toResults (a, b, c, d, e) = HQuintet
    { hfst = toResults @_ @_ @_ @exprs1 a
    , hsnd = toResults @_ @_ @_ @exprs2 b
    , htrd = toResults @_ @_ @_ @exprs3 c
    , hfrt = toResults @_ @_ @_ @exprs4 d
    , hfft = toResults @_ @_ @_ @exprs5 e
    }


instance
  ( HTable t
  , isTabular ~ 'True
  , x ~ t (H Result)
  , y ~ t (H DB)
  ) => ExprsFor 'HTable isTabular x y
 where
  fromResults = id
  toResults = id


instance
  ( Recontextualize DB Result (t DB) (t Result)
  , isTabular ~ 'True
  , x ~ t Result
  , y ~ t DB
  ) => ExprsFor 'Rel8able isTabular x y
 where
  fromResults = fromColumns
  toResults = toColumns


instance
  ( KnownSpec spec
  , isTabular ~ 'True
  , x ~ Result spec
  , y ~ DB spec
  ) => ExprsFor 'Field isTabular x y
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
lit = fromColumns . litTable . toResults @_ @_ @_ @exprs


parse :: forall exprs a. Serializable exprs a => Hasql.Row a
parse = fromResults @_ @_ @_ @exprs <$> parseTable


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
