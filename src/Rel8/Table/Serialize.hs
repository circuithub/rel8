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
{-# language ViewPatterns #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Serialize
  ( Serializable, lit, parse
  , Encodes, litTable
  )
where

-- base
import Data.Bifunctor ( bimap )
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Expr.Serialize ( slitExpr, sparseValue )
import Rel8.Schema.Context ( Col(..) )
import Rel8.Schema.Context.Identity
  ( fromHEitherTable, toHEitherTable
  , fromHListTable, toHListTable
  , fromHMaybeTable, toHMaybeTable
  , fromHNonEmptyTable, toHNonEmptyTable
  , fromHTheseTable, toHTheseTable
  )
import Rel8.Schema.Context.Label ( labeler, unlabeler )
import Rel8.Schema.HTable ( HTable, htabulate, htabulateA, hfield, hspecs )
import Rel8.Schema.HTable.Label ( hlabel, hunlabel )
import Rel8.Schema.HTable.Quartet ( HQuartet(..) )
import Rel8.Schema.HTable.Quintet ( HQuintet(..) )
import Rel8.Schema.HTable.Pair ( HPair(..) )
import Rel8.Schema.HTable.Trio ( HTrio(..) )
import Rel8.Schema.HTable.Type ( HType(..) )
import Rel8.Schema.Nullability ( NotNull, Sql )
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


fromIdentity' :: forall exprs a. ToExprs a exprs => Columns exprs (Col Identity) -> a
fromIdentity' = fromIdentity @_ @exprs


toIdentity' :: forall exprs a. ToExprs a exprs => a -> Columns exprs (Col Identity)
toIdentity' = toIdentity @_ @exprs


type ToExprs :: Type -> Type -> Constraint
class Table Expr exprs => ToExprs a exprs where
  fromIdentity :: Columns exprs (Col Identity) -> a
  toIdentity :: a -> Columns exprs (Col Identity)


instance {-# OVERLAPPABLE #-} (Sql DBType a, x ~ Expr a) => ToExprs a x where
  fromIdentity (HType (Result a)) = a
  toIdentity = HType . Result


instance (Sql DBType a, x ~ [a]) => ToExprs [a] (Expr x) where
  fromIdentity (HType (Result a)) = a
  toIdentity = HType . Result


instance (Sql DBType a, NotNull a, x ~ Maybe a) => ToExprs (Maybe a) (Expr x)
 where
  fromIdentity (HType (Result a)) = a
  toIdentity = HType . Result


instance (Sql DBType a, NotNull a, x ~ NonEmpty a) => ToExprs (NonEmpty a) (Expr x)
 where
  fromIdentity (HType (Result a)) = a
  toIdentity = HType . Result


instance (ToExprs a exprs1, ToExprs b exprs2, x ~ EitherTable exprs1 exprs2) =>
  ToExprs (Either a b) x
 where
  fromIdentity =
    bimap
      (fromIdentity' @exprs1 . hunlabel unlabeler)
      (fromIdentity' @exprs2 . hunlabel unlabeler) .
    fromHEitherTable
  toIdentity =
    toHEitherTable .
    bimap
      (hlabel labeler . toIdentity' @exprs1)
      (hlabel labeler . toIdentity' @exprs2)


instance ToExprs a exprs => ToExprs [a] (ListTable exprs) where
  fromIdentity = fmap (fromIdentity' @exprs) . fromHListTable
  toIdentity = toHListTable . fmap (toIdentity' @exprs)


instance ToExprs a exprs => ToExprs (Maybe a) (MaybeTable exprs) where
  fromIdentity =
    fmap (fromIdentity' @exprs . hunlabel unlabeler) .
    fromHMaybeTable
  toIdentity =
    toHMaybeTable .
    fmap (hlabel labeler . toIdentity' @exprs)


instance ToExprs a exprs => ToExprs (NonEmpty a) (NonEmptyTable exprs)
 where
  fromIdentity = fmap (fromIdentity' @exprs) . fromHNonEmptyTable
  toIdentity = toHNonEmptyTable . fmap (toIdentity' @exprs)


instance (ToExprs a exprs1, ToExprs b exprs2, x ~ TheseTable exprs1 exprs2) =>
  ToExprs (These a b) x
 where
  fromIdentity =
    bimap
      (fromIdentity' @exprs1 . hunlabel unlabeler)
      (fromIdentity' @exprs2 . hunlabel unlabeler) .
    fromHTheseTable
  toIdentity =
    toHTheseTable .
    bimap
      (hlabel labeler . toIdentity' @exprs1)
      (hlabel labeler . toIdentity' @exprs2)


instance (ToExprs a exprs1, ToExprs b exprs2, x ~ (exprs1, exprs2)) =>
  ToExprs (a, b) x
 where
  fromIdentity (HPair a b) =
    ( fromIdentity' @exprs1 $ hunlabel unlabeler a
    , fromIdentity' @exprs2 $ hunlabel unlabeler b
    )
  toIdentity (a, b) = HPair
    { hfst = hlabel labeler $ toIdentity' @exprs1 a
    , hsnd = hlabel labeler $ toIdentity' @exprs2 b
    }


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , ToExprs c exprs3
  , x ~ (exprs1, exprs2, exprs3)
  ) => ToExprs (a, b, c) x
 where
  fromIdentity (HTrio a b c) =
    ( fromIdentity' @exprs1 $ hunlabel unlabeler a
    , fromIdentity' @exprs2 $ hunlabel unlabeler b
    , fromIdentity' @exprs3 $ hunlabel unlabeler c
    )
  toIdentity (a, b, c) = HTrio
    { hfst = hlabel labeler $ toIdentity' @exprs1 a
    , hsnd = hlabel labeler $ toIdentity' @exprs2 b
    , htrd = hlabel labeler $ toIdentity' @exprs3 c
    }


instance
  ( ToExprs a exprs1
  , ToExprs b exprs2
  , ToExprs c exprs3
  , ToExprs d exprs4
  , x ~ (exprs1, exprs2, exprs3, exprs4)
  ) => ToExprs (a, b, c, d) x
 where
  fromIdentity (HQuartet a b c d) =
    ( fromIdentity' @exprs1 $ hunlabel unlabeler a
    , fromIdentity' @exprs2 $ hunlabel unlabeler b
    , fromIdentity' @exprs3 $ hunlabel unlabeler c
    , fromIdentity' @exprs4 $ hunlabel unlabeler d
    )
  toIdentity (a, b, c, d) = HQuartet
    { hfst = hlabel labeler $ toIdentity' @exprs1 a
    , hsnd = hlabel labeler $ toIdentity' @exprs2 b
    , htrd = hlabel labeler $ toIdentity' @exprs3 c
    , hfrt = hlabel labeler $ toIdentity' @exprs4 d
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
  fromIdentity (HQuintet a b c d e) =
    ( fromIdentity' @exprs1 $ hunlabel unlabeler a
    , fromIdentity' @exprs2 $ hunlabel unlabeler b
    , fromIdentity' @exprs3 $ hunlabel unlabeler c
    , fromIdentity' @exprs4 $ hunlabel unlabeler d
    , fromIdentity' @exprs5 $ hunlabel unlabeler e
    )
  toIdentity (a, b, c, d, e) = HQuintet
    { hfst = hlabel labeler $ toIdentity' @exprs1 a
    , hsnd = hlabel labeler $ toIdentity' @exprs2 b
    , htrd = hlabel labeler $ toIdentity' @exprs3 c
    , hfrt = hlabel labeler $ toIdentity' @exprs4 d
    , hfft = hlabel labeler $ toIdentity' @exprs5 e
    }


instance (HTable t, result ~ Col Identity, x ~ t (Col Expr)) =>
  ToExprs (t result) x
 where
  fromIdentity = id
  toIdentity = id


instance (Recontextualize Identity Expr (t Identity) (t Expr), result ~ Identity, x ~ t Expr) =>
  ToExprs (t result) x
 where
  fromIdentity = fromColumns
  toIdentity = toColumns


instance (KnownSpec spec, x ~ Col Expr spec) =>
  ToExprs (Col Identity spec) x
 where
  fromIdentity = fromColumns
  toIdentity = toColumns


type FromExprs :: Type -> Type
type family FromExprs a where
  FromExprs (Expr a) = a
  FromExprs (Col Expr spec) = Col Identity spec
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
  FromExprs (t Expr) = t Identity
  FromExprs (t (Col Expr)) = t (Col Identity)


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
lit = fromColumns . litHTable . toIdentity' @exprs


parse :: forall exprs a. Serializable exprs a => Hasql.Row a
parse = fromIdentity' @exprs <$> parseHTable


type Encodes :: Type -> Type -> Constraint
class Serializable exprs a => Encodes a exprs | a -> exprs, exprs -> a


instance KnownSpec spec => Encodes (Col Identity spec) (Col Expr spec)


instance Serializable (t Identity) (t Expr) => Encodes (t Expr) (t Identity)


instance HTable t => Encodes (t (Col Identity)) (t (Col Expr))


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


litHTable :: HTable t => t (Col Identity) -> t (Col Expr)
litHTable as = htabulate $ \field ->
  case hfield hspecs field of
    SSpec {nullability, info} -> case hfield as field of
      Result value -> DB (slitExpr nullability info value)


parseHTable :: HTable t => Hasql.Row (t (Col Identity))
parseHTable = unwrapApplicative $ htabulateA $ \field ->
  WrapApplicative $ case hfield hspecs field of
    SSpec {nullability, info} -> Result <$> sparseValue nullability info
