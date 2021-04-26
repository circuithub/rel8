{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
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
import GHC.Generics
  ( Generic, Rep, from, to
  , (:*:)( (:*:) ), K1( K1 ), M1( M1 ), Meta( MetaSel ), D, C, S
  )
import GHC.TypeLits ( KnownSymbol )
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql

-- rel8
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Expr.Serialize ( slitExpr, sparseValue )
import Rel8.Generic.Record ( Record(..) )
import Rel8.Generic.Table ( GColumns )
import Rel8.Schema.Context ( Col(..) )
import Rel8.Schema.Context.Label ( labeler, unlabeler )
import Rel8.Schema.HTable ( HTable, htabulate, htabulateA, hfield, hspecs )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Product ( HProduct(..) )
import Rel8.Schema.HTable.Type ( HType(..) )
import Rel8.Schema.Null ( NotNull, Sql )
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Spec ( SSpec(..), KnownSpec )
import Rel8.Table ( Table, Columns, fromColumns, toColumns, TColumns )
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


type ToExprs :: Type -> Type -> Constraint
class Table Expr exprs => ToExprs exprs a where
  fromResult :: Columns exprs (Col Result) -> a
  toResult :: a -> Columns exprs (Col Result)

  default fromResult ::
    ( Generic (Record a)
    , GToExprs (Rep (Record exprs)) (Rep (Record a))
    , Columns exprs ~ GColumns TColumns (Rep (Record exprs))
    )
    => Columns exprs (Col Result) -> a
  fromResult = unrecord . to . gfromResult @(Rep (Record exprs))

  default toResult ::
    ( Generic (Record a)
    , GToExprs (Rep (Record exprs)) (Rep (Record a))
    , Columns exprs ~ GColumns TColumns (Rep (Record exprs))
    )
    => a -> Columns exprs (Col Result)
  toResult = gtoResult @(Rep (Record exprs)) . from . Record


type GToExprs :: (Type -> Type) -> (Type -> Type) -> Constraint
class GToExprs exprs rep where
  gfromResult :: GColumns TColumns exprs (Col Result) -> rep x
  gtoResult :: rep x -> GColumns TColumns exprs (Col Result)


instance GToExprs exprs rep => GToExprs (M1 D c exprs) (M1 D c rep) where
  gfromResult = M1 . gfromResult @exprs
  gtoResult (M1 a) = gtoResult @exprs a


instance GToExprs exprs rep => GToExprs (M1 C c exprs) (M1 C c rep) where
  gfromResult = M1 . gfromResult @exprs
  gtoResult (M1 a) = gtoResult @exprs a


instance (GToExprs exprs1 rep1, GToExprs exprs2 rep2) =>
  GToExprs (exprs1 :*: exprs2) (rep1 :*: rep2)
 where
  gfromResult (HProduct a b) = gfromResult @exprs1 a :*: gfromResult @exprs2 b
  gtoResult (a :*: b) = HProduct (gtoResult @exprs1 a) (gtoResult @exprs2 b)


instance
  ( ToExprs exprs a
  , KnownSymbol label
  , GColumns TColumns (M1 S meta k1) ~ HLabel label (Columns exprs)
  , meta ~ 'MetaSel ('Just label) _su _ss _ds
  , k1 ~ K1 i exprs
  , k1' ~ K1 i a
  )
  => GToExprs (M1 S meta k1) (M1 S meta k1')
 where
  gfromResult = M1 . K1 . fromResult @exprs . hunlabel unlabeler
  gtoResult (M1 (K1 a)) = hlabel labeler (toResult @exprs a)


instance {-# OVERLAPPABLE #-} (Sql DBType a, x ~ Expr a) => ToExprs x a where
  fromResult (HType (Result a)) = a
  toResult = HType . Result


instance (Sql DBType a, x ~ [a]) => ToExprs (Expr x) [a] where
  fromResult (HType (Result a)) = a
  toResult = HType . Result


instance (Sql DBType a, NotNull a, x ~ Maybe a) => ToExprs (Expr x) (Maybe a)
 where
  fromResult (HType (Result a)) = a
  toResult = HType . Result


instance (Sql DBType a, NotNull a, x ~ NonEmpty a) => ToExprs (Expr x) (NonEmpty a)
 where
  fromResult (HType (Result a)) = a
  toResult = HType . Result


instance (ToExprs exprs1 a, ToExprs exprs2 b, x ~ EitherTable exprs1 exprs2) =>
  ToExprs x (Either a b)
 where
  fromResult =
    bimap (fromResult @exprs1) (fromResult @exprs2) .
    fromColumns
  toResult =
    toColumns .
    bimap (toResult @exprs1) (toResult @exprs2)


instance ToExprs exprs a => ToExprs (ListTable exprs) [a] where
  fromResult = fmap (fromResult @exprs) . fromColumns
  toResult = toColumns . fmap (toResult @exprs)


instance ToExprs exprs a => ToExprs (MaybeTable exprs) (Maybe a) where
  fromResult = fmap (fromResult @exprs) . fromColumns
  toResult = toColumns . fmap (toResult @exprs)


instance ToExprs exprs a => ToExprs (NonEmptyTable exprs) (NonEmpty a)
 where
  fromResult = fmap (fromResult @exprs) . fromColumns
  toResult = toColumns . fmap (toResult @exprs)


instance (ToExprs exprs1 a, ToExprs exprs2 b, x ~ TheseTable exprs1 exprs2) =>
  ToExprs x (These a b)
 where
  fromResult =
    bimap (fromResult @exprs1) (fromResult @exprs2) .
    fromColumns
  toResult =
    toColumns .
    bimap (toResult @exprs1) (toResult @exprs2)


instance (ToExprs exprs1 a, ToExprs exprs2 b, x ~ (exprs1, exprs2)) =>
  ToExprs x (a, b)


instance
  ( ToExprs exprs1 a
  , ToExprs exprs2 b
  , ToExprs exprs3 c
  , x ~ (exprs1, exprs2, exprs3)
  )
  => ToExprs x (a, b, c)


instance
  ( ToExprs exprs1 a
  , ToExprs exprs2 b
  , ToExprs exprs3 c
  , ToExprs exprs4 d
  , x ~ (exprs1, exprs2, exprs3, exprs4)
  )
  => ToExprs x (a, b, c, d)


instance
  ( ToExprs exprs1 a
  , ToExprs exprs2 b
  , ToExprs exprs3 c
  , ToExprs exprs4 d
  , ToExprs exprs5 e
  , x ~ (exprs1, exprs2, exprs3, exprs4, exprs5)
  )
  => ToExprs x (a, b, c, d, e)


instance
  ( ToExprs exprs1 a
  , ToExprs exprs2 b
  , ToExprs exprs3 c
  , ToExprs exprs4 d
  , ToExprs exprs5 e
  , ToExprs exprs6 f
  , x ~ (exprs1, exprs2, exprs3, exprs4, exprs5, exprs6)
  )
  => ToExprs x (a, b, c, d, e, f)


instance
  ( ToExprs exprs1 a
  , ToExprs exprs2 b
  , ToExprs exprs3 c
  , ToExprs exprs4 d
  , ToExprs exprs5 e
  , ToExprs exprs6 f
  , ToExprs exprs7 g
  , x ~ (exprs1, exprs2, exprs3, exprs4, exprs5, exprs6, exprs7)
  )
  => ToExprs x (a, b, c, d, e, f, g)


instance (HTable t, result ~ Col Result, x ~ t (Col Expr)) =>
  ToExprs x (t result)
 where
  fromResult = id
  toResult = id


instance (Recontextualize Result Expr (t Result) (t Expr), result ~ Result, x ~ t Expr) =>
  ToExprs x (t result)
 where
  fromResult = fromColumns
  toResult = toColumns


instance (KnownSpec spec, x ~ Col Expr spec) =>
  ToExprs x (Col Result spec)
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
type instance FromExprs (a, b, c, d, e, f) =
  ( FromExprs a, FromExprs b, FromExprs c, FromExprs d, FromExprs e
  , FromExprs f
  )
type instance FromExprs (a, b, c, d, e, f, g) =
  ( FromExprs a, FromExprs b, FromExprs c, FromExprs d, FromExprs e
  , FromExprs f, FromExprs g
  )
type instance FromExprs (t Expr) = t Result
type instance FromExprs (t (Col Expr)) = t (Col Result)


-- | @Serializable@ witnesses the one-to-one correspondence between the type
-- @sql@, which contains SQL expressions, and the type @haskell@, which
-- contains the Haskell decoding of rows containing @sql@ SQL expressions.
type Serializable :: Type -> Type -> Constraint
class (ToExprs exprs a, a ~ FromExprs exprs) => Serializable exprs a | exprs -> a
instance (ToExprs exprs a, a ~ FromExprs exprs) => Serializable exprs a
instance {-# OVERLAPPING #-} Sql DBType a => Serializable (Expr a) a


-- | Use @lit@ to turn literal Haskell values into expressions. @lit@ is
-- capable of lifting single @Expr@s to full tables.
lit :: forall exprs a. Serializable exprs a => a -> exprs
lit = fromColumns . litHTable . toResult @exprs


parse :: forall exprs a. Serializable exprs a => Hasql.Row a
parse = fromResult @exprs <$> parseHTable


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
