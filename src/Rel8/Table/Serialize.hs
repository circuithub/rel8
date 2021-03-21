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
  , FromDBType, ToDBType
  , FromType, ToType
  )
import Rel8.Kind.Emptiability ( Emptiability( Emptiable, NonEmptiable ) )
import Rel8.Kind.Nullability
  ( Nullability( Nullable, NonNullable )
  , KnownNullability
  )
import Rel8.Opaque ( Opaque )
import Rel8.Schema.Context ( DB(..), Result(..) )
import Rel8.Schema.Context.Label ( labeler, unlabeler )
import Rel8.Schema.Context.Result
  ( fromHEitherTable, toHEitherTable
  , fromHListTable, toHListTable
  , fromHMaybeTable, toHMaybeTable
  , fromHNonEmptyTable, toHNonEmptyTable
  , fromHTheseTable, toHTheseTable
  )
import Rel8.Schema.HTable ( HTable, htabulate, htabulateA, hfield, hspecs )
import Rel8.Schema.HTable.DBType ( HDBType(..) )
import Rel8.Schema.HTable.Label ( hlabel, hunlabel )
import Rel8.Schema.HTable.Quartet ( HQuartet(..) )
import Rel8.Schema.HTable.Quintet ( HQuintet(..) )
import Rel8.Schema.HTable.Pair ( HPair(..) )
import Rel8.Schema.HTable.Trio ( HTrio(..) )
import Rel8.Schema.HTable.Context ( H )
import Rel8.Schema.Spec ( SSpec( SSpec ), KnownSpec )
import Rel8.Schema.Value
  ( Value( NullableValue, NonNullableValue )
  , FromValue, ToValue
  )
import Rel8.Table ( Table, Columns, fromColumns, toColumns )
import Rel8.Table.Either ( EitherTable )
import Rel8.Table.List ( ListTable )
import Rel8.Table.Maybe ( MaybeTable )
import Rel8.Table.NonEmpty ( NonEmptyTable )
import Rel8.Table.Recontextualize ( Encodes )
import Rel8.Table.These ( TheseTable )
import Rel8.Type ( DBType )
import Rel8.Type.Array ( Array )

-- semigroupoids
import Data.Functor.Apply ( WrappedApplicative(..) )

-- these
import Data.These ( These )


type IsPlainColumn :: Type -> Bool
type family IsPlainColumn a where
  IsPlainColumn (Either _ _) = 'False
  IsPlainColumn [_] = 'False
  IsPlainColumn (Maybe _) = 'False
  IsPlainColumn (NonEmpty _) = 'False
  IsPlainColumn (These _ _) = 'False
  IsPlainColumn (_, _) = 'False
  IsPlainColumn (_, _, _) = 'False
  IsPlainColumn (_, _, _, _) = 'False
  IsPlainColumn (_, _, _, _, _) = 'False
  IsPlainColumn (_ (H Result)) = 'False
  IsPlainColumn (_ Result) = 'False
  IsPlainColumn (Result _) = 'False
  IsPlainColumn _ = 'True


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


type IsTabular' :: Bool -> Type -> Bool
type family IsTabular' isTabular exprs where
  IsTabular' _ (Expr _ _) = 'False
  IsTabular' 'False _ = 'False
  IsTabular' _ _ = 'True


type IsMaybeTabular :: Type -> Bool
type family IsMaybeTabular a where
  IsMaybeTabular (Maybe _) = 'True
  IsMaybeTabular a = IsTabular a


type IsListTabular :: Type -> Bool
type family IsListTabular a where
  IsListTabular (Maybe a) = IsMaybeTabular a
  IsListTabular a = IsTabular a


type ToExprs :: Type -> Type -> Constraint
class ExprsFor (IsPlainColumn a) (IsTabular' (IsTabular a) exprs) a exprs => ToExprs a exprs
instance ExprsFor (IsPlainColumn a) (IsTabular' (IsTabular a) exprs) a exprs => ToExprs a exprs


fromResults' :: forall exprs a. ToExprs a exprs => Columns exprs (H Result) -> a
fromResults' = fromResults @(IsPlainColumn a) @_ @_ @exprs


toResults' :: forall exprs a. ToExprs a exprs => a -> Columns exprs (H Result)
toResults' = toResults @(IsPlainColumn a) @_ @_ @exprs


type ExprsFor :: Bool -> Bool -> Type -> Type -> Constraint
class (Table DB exprs, isTabular ~ IsTabular a) =>
  ExprsFor isPlainColumn isTabular a exprs where
  fromResults :: Columns exprs (H Result) -> a
  toResults :: a -> Columns exprs (H Result)


instance
  ( exprs ~ Expr 'NonNullable dbType
  , ToDBType (FromType a) ~ dbType
  , ToType (FromDBType dbType) ~ a
  , DBType dbType
  , FromDBType a ~ 'Scalar a
  , IsTabular a ~ 'False
  ) => ExprsFor 'True 'False a exprs
 where
  fromResults (HDBType (Result (NonNullableValue a))) = a
  toResults = HDBType . Result . NonNullableValue


instance
  ( '(nullability, a) ~ ToValue ma
  , FromValue nullability a ~ ma
  , ToDBType (FromType a) ~ dbType
  , ToType (FromDBType dbType) ~ a
  , DBType dbType
  , KnownNullability nullability
  , IsListTabular ma ~ 'False
  , x ~ Array 'Emptiable nullability dbType
  , outerNullability ~ 'NonNullable
  ) => ExprsFor 'False 'False [ma] (Expr outerNullability x)
 where
  fromResults (HDBType (Result (NonNullableValue a))) = a
  toResults = HDBType . Result . NonNullableValue


instance
  ( nullability ~ 'Nullable
  , ToDBType (FromType a) ~ dbType
  , ToType (FromDBType dbType) ~ a
  , DBType dbType
  , isTabular ~ 'False
  , IsMaybeTabular a ~ 'False
  ) => ExprsFor 'False 'False (Maybe a) (Expr nullability dbType)
 where
  fromResults (HDBType (Result (NullableValue a))) = a
  toResults = HDBType . Result . NullableValue


instance
  ( '(nullability, a) ~ ToValue ma
  , FromValue nullability a ~ ma
  , ToDBType (FromType a) ~ dbType
  , ToType (FromDBType dbType) ~ a
  , DBType dbType
  , KnownNullability nullability
  , IsListTabular ma ~ 'False
  , x ~ Array 'NonEmptiable nullability dbType
  , outerNullability ~ 'NonNullable
  ) => ExprsFor 'False 'False (NonEmpty ma) (Expr outerNullability x)
 where
  fromResults (HDBType (Result (NonNullableValue a))) = a
  toResults = HDBType . Result . NonNullableValue


instance
  ( ToExprs a exprs
  , IsListTabular a ~ 'False
  ) => ExprsFor 'False 'False [a] (ListTable exprs)
 where
  fromResults = fmap (fromResults' @exprs) . fromHListTable
  toResults = toHListTable . fmap (toResults' @exprs)


instance
  ( ToExprs a exprs
  , IsMaybeTabular a ~ 'False
  ) => ExprsFor 'False 'False (Maybe a) (MaybeTable exprs)
 where
  fromResults =
    fmap (fromResults' @exprs . hunlabel unlabeler) .
    fromHMaybeTable
  toResults =
    toHMaybeTable .
    fmap (hlabel labeler . toResults' @exprs)


instance
  ( ToExprs a exprs
  , IsListTabular a ~ 'False
  ) => ExprsFor 'False 'False (NonEmpty a) (NonEmptyTable exprs)
 where
  fromResults = fmap (fromResults' @exprs) . fromHNonEmptyTable
  toResults = toHNonEmptyTable . fmap (toResults' @exprs)


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , isTabular ~ 'True
  , x ~ EitherTable exprs1 exprs2
  ) => ExprsFor 'False isTabular (Either a b) x
 where
  fromResults =
    bimap
      (fromResults' @exprs1 . hunlabel unlabeler)
      (fromResults' @exprs2 . hunlabel unlabeler) .
    fromHEitherTable
  toResults =
    toHEitherTable .
    bimap
      (hlabel labeler . toResults' @exprs1)
      (hlabel labeler . toResults' @exprs2)


instance
  ( ToExprs a exprs
  , IsListTabular a ~ 'True
  , x ~ ListTable exprs
  ) => ExprsFor 'False 'True [a] x
 where
  fromResults = fmap (fromResults' @exprs) . fromHListTable
  toResults = toHListTable . fmap (toResults' @exprs)


instance
  ( ToExprs a exprs
  , IsMaybeTabular a ~ 'True
  , x ~ MaybeTable exprs
  ) => ExprsFor 'False 'True (Maybe a) x
 where
  fromResults =
    fmap (fromResults' @exprs . hunlabel unlabeler) .
    fromHMaybeTable
  toResults =
    toHMaybeTable .
    fmap (hlabel labeler . toResults' @exprs)


instance
  ( ToExprs a exprs
  , IsListTabular a ~ 'True
  , x ~ NonEmptyTable exprs
  ) => ExprsFor 'False 'True (NonEmpty a) x
 where
  fromResults = fmap (fromResults' @exprs) . fromHNonEmptyTable
  toResults = toHNonEmptyTable . fmap (toResults' @exprs)


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , isTabular ~ 'True
  , x ~ TheseTable exprs1 exprs2
  ) => ExprsFor 'False isTabular (These a b) x
 where
  fromResults =
    bimap
      (fromResults' @exprs1 . hunlabel unlabeler)
      (fromResults' @exprs2 . hunlabel unlabeler) .
    fromHTheseTable
  toResults =
    toHTheseTable .
    bimap
      (hlabel labeler . toResults' @exprs1)
      (hlabel labeler . toResults' @exprs2)


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , isTabular ~ 'True
  , x ~ (exprs1, exprs2)
  ) => ExprsFor 'False isTabular (a, b) x
 where
  fromResults (HPair a b) =
    ( fromResults' @exprs1 $ hunlabel unlabeler a
    , fromResults' @exprs2 $ hunlabel unlabeler b
    )
  toResults (a, b) = HPair
    { hfst = hlabel labeler $ toResults' @exprs1 a
    , hsnd = hlabel labeler $ toResults' @exprs2 b
    }


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , ToExprs c exprs3
  , isTabular ~ 'True
  , x ~ (exprs1, exprs2, exprs3)
  ) => ExprsFor 'False isTabular (a, b, c) x
 where
  fromResults (HTrio a b c) =
    ( fromResults' @exprs1 $ hunlabel unlabeler a
    , fromResults' @exprs2 $ hunlabel unlabeler b
    , fromResults' @exprs3 $ hunlabel unlabeler c
    )
  toResults (a, b, c) = HTrio
    { hfst = hlabel labeler $ toResults' @exprs1 a
    , hsnd = hlabel labeler $ toResults' @exprs2 b
    , htrd = hlabel labeler $ toResults' @exprs3 c
    }


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , ToExprs c exprs3
  , ToExprs d exprs4
  , isTabular ~ 'True
  , x ~ (exprs1, exprs2, exprs3, exprs4)
  ) => ExprsFor 'False isTabular (a, b, c, d) x
 where
  fromResults (HQuartet a b c d) =
    ( fromResults' @exprs1 $ hunlabel unlabeler a
    , fromResults' @exprs2 $ hunlabel unlabeler b
    , fromResults' @exprs3 $ hunlabel unlabeler c
    , fromResults' @exprs4 $ hunlabel unlabeler d
    )
  toResults (a, b, c, d) = HQuartet
    { hfst = hlabel labeler $ toResults' @exprs1 a
    , hsnd = hlabel labeler $ toResults' @exprs2 b
    , htrd = hlabel labeler $ toResults' @exprs3 c
    , hfrt = hlabel labeler $ toResults' @exprs4 d
    }


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , ToExprs c exprs3
  , ToExprs d exprs4
  , ToExprs e exprs5
  , isTabular ~ 'True
  , x ~ (exprs1, exprs2, exprs3, exprs4, exprs5)
  ) => ExprsFor 'False isTabular (a, b, c, d, e) x
 where
  fromResults (HQuintet a b c d e) =
    ( fromResults' @exprs1 $ hunlabel unlabeler a
    , fromResults' @exprs2 $ hunlabel unlabeler b
    , fromResults' @exprs3 $ hunlabel unlabeler c
    , fromResults' @exprs4 $ hunlabel unlabeler d
    , fromResults' @exprs5 $ hunlabel unlabeler e
    )
  toResults (a, b, c, d, e) = HQuintet
    { hfst = hlabel labeler $ toResults' @exprs1 a
    , hsnd = hlabel labeler $ toResults' @exprs2 b
    , htrd = hlabel labeler $ toResults' @exprs3 c
    , hfrt = hlabel labeler $ toResults' @exprs4 d
    , hfft = hlabel labeler $ toResults' @exprs5 e
    }


instance
  ( HTable t
  , isTabular ~ 'True
  , result ~ H Result
  , x ~ t (H DB)
  ) => ExprsFor 'False isTabular (t result) x
 where
  fromResults = id
  toResults = id


instance
  ( Encodes (t Result) (t DB)
  , isTabular ~ 'True
  , result ~ Result
  , x ~ t DB
  ) => ExprsFor 'False isTabular (t result) x
 where
  fromResults = fromColumns
  toResults = toColumns


instance
  ( KnownSpec spec
  , isTabular ~ 'True
  , x ~ DB spec
  ) => ExprsFor 'False isTabular (Result spec) x
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
instance Serializable (Expr 'NonNullable Opaque) Opaque


lit :: forall exprs a. Serializable exprs a => a -> exprs
lit = fromColumns . litTable . toResults' @exprs


parse :: forall a exprs. Serializable exprs a => Hasql.Row a
parse = fromResults' @exprs <$> parseTable


litTable :: Encodes a b => a -> b
litTable (toColumns -> as) = fromColumns $ htabulate $ \field ->
  case hfield hspecs field of
    SSpec _ _ _ blueprint -> case hfield as field of
      Result value -> DB (slitExpr blueprint value)


parseTable :: Table Result a => Hasql.Row a
parseTable = fmap fromColumns $ unwrapApplicative $ htabulateA $ \field ->
  WrapApplicative $ case hfield hspecs field of
    SSpec _ _ nullability blueprint ->
      Result <$> sparseValue nullability blueprint
