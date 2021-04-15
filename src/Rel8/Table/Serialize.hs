{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Serialize
  ( Serializable, lit, parse
  , Encodes, litTable
  , ToExprs(..)
  , FromExprs
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
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Expr.Serialize ( slitExpr, sparseValue )
import Rel8.Schema.Context ( Col(..) )
import Rel8.Schema.Context.Label ( labeler, unlabeler )
import Rel8.Schema.HTable ( HTable, htabulate, htabulateA, hfield, hspecs )
import Rel8.Schema.HTable.Label ( hlabel, hunlabel )
import Rel8.Schema.HTable.Quartet ( HQuartet(..) )
import Rel8.Schema.HTable.Quintet ( HQuintet(..) )
import Rel8.Schema.HTable.Pair ( HPair(..) )
import Rel8.Schema.HTable.Trio ( HTrio(..) )
import Rel8.Schema.HTable.Type ( HType(..) )
import Rel8.Schema.Null ( NotNull, Sql )
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Spec ( SSpec(..), KnownSpec )
import Rel8.Table ( Table, Columns, fromColumns, toColumns )
import Rel8.Table.Either ( EitherTable )
import Rel8.Table.List ( ListTable )
import Rel8.Table.Maybe ( MaybeTable )
import Rel8.Table.NonEmpty ( NonEmptyTable )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.These ( TheseTable )
import Rel8.Type ( DBType )

-- semigroupoids
import Data.Functor.Apply ( WrappedApplicative(..) )

-- these
import Data.These ( These )


fromResult' :: forall exprs a. ToExprs a exprs => Columns exprs (Col Result) -> a
fromResult' = fromResult @_ @exprs


toResult' :: forall exprs a. ToExprs a exprs => a -> Columns exprs (Col Result)
toResult' = toResult @_ @exprs


type ToExprs :: Type -> Type -> Constraint
class Table Expr exprs => ToExprs a exprs where
  fromResult :: Columns exprs (Col Result) -> a
  toResult :: a -> Columns exprs (Col Result)


instance {-# OVERLAPPABLE #-} (Sql DBType a, x ~ Expr a) => ToExprs a x where
  fromResult (HType (Result a)) = a
  toResult = HType . Result


instance (Sql DBType a, x ~ [a]) => ToExprs [a] (Expr x) where
  fromResult (HType (Result a)) = a
  toResult = HType . Result


instance (Sql DBType a, NotNull a, x ~ Maybe a) => ToExprs (Maybe a) (Expr x)
 where
  fromResult (HType (Result a)) = a
  toResult = HType . Result


instance (Sql DBType a, NotNull a, x ~ NonEmpty a) => ToExprs (NonEmpty a) (Expr x)
 where
  fromResult (HType (Result a)) = a
  toResult = HType . Result


instance (ToExprs a exprs1, ToExprs b exprs2, x ~ EitherTable exprs1 exprs2) =>
  ToExprs (Either a b) x
 where
  fromResult =
    bimap (fromResult' @exprs1) (fromResult' @exprs2) .
    fromColumns
  toResult =
    toColumns .
    bimap (toResult' @exprs1) (toResult' @exprs2)


instance ToExprs a exprs => ToExprs [a] (ListTable exprs) where
  fromResult = fmap (fromResult' @exprs) . fromColumns
  toResult = toColumns . fmap (toResult' @exprs)


instance ToExprs a exprs => ToExprs (Maybe a) (MaybeTable exprs) where
  fromResult = fmap (fromResult' @exprs) . fromColumns
  toResult = toColumns . fmap (toResult' @exprs)


instance ToExprs a exprs => ToExprs (NonEmpty a) (NonEmptyTable exprs)
 where
  fromResult = fmap (fromResult' @exprs) . fromColumns
  toResult = toColumns . fmap (toResult' @exprs)


instance (ToExprs a exprs1, ToExprs b exprs2, x ~ TheseTable exprs1 exprs2) =>
  ToExprs (These a b) x
 where
  fromResult =
    bimap (fromResult' @exprs1) (fromResult' @exprs2) .
    fromColumns
  toResult =
    toColumns .
    bimap (toResult' @exprs1) (toResult' @exprs2)


instance (ToExprs a exprs1, ToExprs b exprs2, x ~ (exprs1, exprs2)) =>
  ToExprs (a, b) x
 where
  fromResult (HPair a b) =
    ( fromResult' @exprs1 $ hunlabel unlabeler a
    , fromResult' @exprs2 $ hunlabel unlabeler b
    )
  toResult (a, b) = HPair
    { hfst = hlabel labeler $ toResult' @exprs1 a
    , hsnd = hlabel labeler $ toResult' @exprs2 b
    }


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , ToExprs c exprs3
  , x ~ (exprs1, exprs2, exprs3)
  ) => ToExprs (a, b, c) x
 where
  fromResult (HTrio a b c) =
    ( fromResult' @exprs1 $ hunlabel unlabeler a
    , fromResult' @exprs2 $ hunlabel unlabeler b
    , fromResult' @exprs3 $ hunlabel unlabeler c
    )
  toResult (a, b, c) = HTrio
    { hfst = hlabel labeler $ toResult' @exprs1 a
    , hsnd = hlabel labeler $ toResult' @exprs2 b
    , htrd = hlabel labeler $ toResult' @exprs3 c
    }


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , ToExprs c exprs3
  , ToExprs d exprs4
  , x ~ (exprs1, exprs2, exprs3, exprs4)
  ) => ToExprs (a, b, c, d) x
 where
  fromResult (HQuartet a b c d) =
    ( fromResult' @exprs1 $ hunlabel unlabeler a
    , fromResult' @exprs2 $ hunlabel unlabeler b
    , fromResult' @exprs3 $ hunlabel unlabeler c
    , fromResult' @exprs4 $ hunlabel unlabeler d
    )
  toResult (a, b, c, d) = HQuartet
    { hfst = hlabel labeler $ toResult' @exprs1 a
    , hsnd = hlabel labeler $ toResult' @exprs2 b
    , htrd = hlabel labeler $ toResult' @exprs3 c
    , hfrt = hlabel labeler $ toResult' @exprs4 d
    }


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , ToExprs c exprs3
  , ToExprs d exprs4
  , ToExprs e exprs5
  , x ~ (exprs1, exprs2, exprs3, exprs4, exprs5)
  ) => ToExprs (a, b, c, d, e) x
 where
  fromResult (HQuintet a b c d e) =
    ( fromResult' @exprs1 $ hunlabel unlabeler a
    , fromResult' @exprs2 $ hunlabel unlabeler b
    , fromResult' @exprs3 $ hunlabel unlabeler c
    , fromResult' @exprs4 $ hunlabel unlabeler d
    , fromResult' @exprs5 $ hunlabel unlabeler e
    )
  toResult (a, b, c, d, e) = HQuintet
    { hfst = hlabel labeler $ toResult' @exprs1 a
    , hsnd = hlabel labeler $ toResult' @exprs2 b
    , htrd = hlabel labeler $ toResult' @exprs3 c
    , hfrt = hlabel labeler $ toResult' @exprs4 d
    , hfft = hlabel labeler $ toResult' @exprs5 e
    }


instance (HTable t, result ~ Col Result, x ~ t (Col Expr)) =>
  ToExprs (t result) x
 where
  fromResult = id
  toResult = id


instance (Recontextualize Result Expr (t Result) (t Expr), result ~ Result, x ~ t Expr) =>
  ToExprs (t result) x
 where
  fromResult = fromColumns
  toResult = toColumns


instance (KnownSpec spec, x ~ Col Expr spec) =>
  ToExprs (Col Result spec) x
 where
  fromResult = fromColumns
  toResult = toColumns


type FromExprs :: Type -> Type
type family FromExprs a
type instance FromExprs (Expr a) = a
type instance FromExprs (Col Expr spec) = Col Result spec
type instance FromExprs (EitherTable a b) = Either (FromExprs a) (FromExprs b)
type instance FromExprs (ListTable a) = [FromExprs a]
type instance FromExprs (MaybeTable a) = Maybe (FromExprs a)
type instance FromExprs (NonEmptyTable a) = NonEmpty (FromExprs a)
type instance FromExprs (TheseTable a b) = These (FromExprs a) (FromExprs b)
type instance FromExprs (a, b) = (FromExprs a, FromExprs b)
type instance FromExprs (a, b, c) = (FromExprs a, FromExprs b, FromExprs c)
type instance FromExprs (a, b, c, d) =
  (FromExprs a, FromExprs b, FromExprs c, FromExprs d)
type instance FromExprs (a, b, c, d, e) =
  (FromExprs a, FromExprs b, FromExprs c, FromExprs d, FromExprs e)
type instance FromExprs (t Expr) = t Result
type instance FromExprs (t (Col Expr)) = t (Col Result)


-- | @Serializable@ witnesses the one-to-one correspondence between the type
-- @sql@, which contains SQL expressions, and the type @haskell@, which
-- contains the Haskell decoding of rows containing @sql@ SQL expressions.
type Serializable :: Type -> Type -> Constraint
class (ToExprs a exprs, a ~ FromExprs exprs) => Serializable exprs a | exprs -> a
instance (ToExprs a exprs, a ~ FromExprs exprs) => Serializable exprs a
instance {-# OVERLAPPING #-} Sql DBType a => Serializable (Expr a) a


-- | Use @lit@ to turn literal Haskell values into expressions. @lit@ is
-- capable of lifting single @Expr@s to full tables.
lit :: forall exprs a. Serializable exprs a => a -> exprs
lit = fromColumns . litHTable . toResult' @exprs


parse :: forall exprs a. Serializable exprs a => Hasql.Row a
parse = fromResult' @exprs <$> parseHTable


type Encodes :: Type -> Type -> Constraint
class Serializable exprs a => Encodes a exprs | a -> exprs, exprs -> a


instance KnownSpec spec => Encodes (Col Result spec) (Col Expr spec)


instance Serializable (t Result) (t Expr) => Encodes (t Expr) (t Result)


instance HTable t => Encodes (t (Col Result)) (t (Col Expr))


instance (Encodes a x, Encodes b y) => Encodes (Either a b) (EitherTable x y)


instance Encodes a x => Encodes [a] (ListTable x)


instance Encodes a x => Encodes (Maybe a) (MaybeTable x)


instance Encodes a x => Encodes (NonEmpty a) (NonEmptyTable x)


instance (Encodes a x, Encodes b y) => Encodes (These a b) (TheseTable x y)


instance (Encodes a x, Encodes b y) => Encodes (a, b) (x, y)


instance (Encodes a x, Encodes b y, Encodes c z) =>
  Encodes (a, b, c) (x, y, z)


instance (Encodes a w, Encodes b x, Encodes c y, Encodes d z) =>
  Encodes (a, b, c, d) (w, x, y, z)


instance (Encodes a v, Encodes b w, Encodes c x, Encodes d y, Encodes e z) =>
  Encodes (a, b, c, d, e) (v, w, x, y, z)


litTable :: Encodes a exprs => a -> exprs
litTable = lit


litHTable :: HTable t => t (Col Result) -> t (Col Expr)
litHTable as = htabulate $ \field ->
  case hfield hspecs field of
    SSpec {nullity, info} -> case hfield as field of
      Result value -> DB (slitExpr nullity info value)


parseHTable :: HTable t => Hasql.Row (t (Col Result))
parseHTable = unwrapApplicative $ htabulateA $ \field ->
  WrapApplicative $ case hfield hspecs field of
    SSpec {nullity, info} -> Result <$> sparseValue nullity info
