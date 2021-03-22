{-# LANGUAGE LambdaCase #-}
{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-# options_ghc -Wno-orphans #-}

module Rel8.Generic ( Column, ColumnWithDefault, HList, HMaybe, HNonEmpty, HigherKindedTable ) where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind (Type, Constraint)
import Data.List.NonEmpty ( NonEmpty )
import GHC.Generics ( Generic( Rep, from, to ), K1(K1), M1(M1, unM1), type (:*:)((:*:)), unK1)

-- rel8
import Rel8.Context (unI)
import qualified Rel8.Context as C
import Rel8.Expr ( Expr )
import qualified Rel8.Expr.Instances as E
import Rel8.HTable ( HTable )
import Rel8.HTable.HIdentity ( HIdentity( HIdentity, unHIdentity ) )
import Rel8.HTable.HMapTable ( HMapTable )
import Rel8.HTable.HPair ( HPair(HPair) )
import Rel8.Serializable ( Serializable, ExprFor( unpack, pack ) )
import Rel8.Statement.Insert (Insert, Inserts)
import qualified Rel8.Statement.Insert as I
import Rel8.Table ( Table(Columns, fromColumns, toColumns), AllColumns )
import Rel8.Table.ListTable ( ListTable, ListOf )
import Rel8.Table.MaybeTable ( HMaybeTable, MaybeTable )
import Rel8.Table.NonEmptyTable ( NonEmptyTable, NonEmptyList )
import Rel8.Table.Selects ( Selects )
import Rel8.TableSchema.ColumnSchema ( ColumnSchema )
import qualified Rel8.TableSchema.ColumnSchema as E


-- | The @Column@ type family should be used to indicate which fields of your
-- data types are single columns in queries. This type family has special
-- support when a query is executed, allowing you to use a single data type for
-- both query data and rows decoded to Haskell.
-- 
-- To understand why this type family is special, let's consider a simple
-- higher-kinded data type of Haskell packages:
-- 
-- >>> :{
-- data Package f = Package
--   { packageName   :: Column f Text
--   , packageAuthor :: Column f Text
--   }
-- :}
-- 
-- In queries, @f@ will be some type of 'Expr', and @Column Expr a@ reduces to
-- just @Expr a@:
-- 
-- >>> :t packageName (undefined :: Package Expr)
-- packageName (undefined :: Package Expr) :: Expr Text
-- 
-- When we 'select' queries of this type, @f@ will be instantiated as
-- @Identity@, at which point all wrapping entire disappears:
-- 
-- >>> :t packageName (undefined :: Package Identity)
-- packageName (undefined :: Package Identity) :: Text
-- 
-- In @rel8@ we try hard to always know what @f@ is, which means holes should
-- mention precise types, rather than the @Column@ type family. You should only
-- need to be aware of the type family when defining your table types.
type family Column (context :: (Type -> Type)) (a :: Type) :: Type where
  Column Identity a = a
  Column Insert a   = Expr a
  Column f a        = f a


type family ColumnWithDefault (context :: (Type -> Type)) (a :: Type) :: Type where
  ColumnWithDefault Identity a = a
  ColumnWithDefault Insert   a = Maybe (Expr a)
  ColumnWithDefault f        a = f a


type family HMaybe (context :: Type -> Type) (a :: Type) :: Type where
  HMaybe Identity a = Maybe a
  HMaybe Expr a     = MaybeTable a
  HMaybe f a        = HMaybeTable (Columns a) (C.Column f)


type family HList (context :: Type -> Type) (a :: Type) :: Type where
  HList Identity a = [a]
  HList Expr a     = ListTable a
  HList f a        = HMapTable ListOf (Columns a) (C.Column f)


type family HNonEmpty (context :: Type -> Type) (a :: Type) :: Type where
  HNonEmpty Identity a = NonEmpty a
  HNonEmpty Expr a     = NonEmptyTable a
  HNonEmpty f a        = HMapTable NonEmptyList (Columns a) (C.Column f)


-- | Higher-kinded data types.
--
-- Higher-kinded data types are data types of the pattern:
--
-- @
-- data MyType f =
--   MyType { field1 :: Column f T1 OR HK1 f
--          , field2 :: Column f T2 OR HK2 f
--          , ...
--          , fieldN :: Column f Tn OR HKn f
--          }
-- @
-- 
-- where @Tn@ is any Haskell type, and @HKn@ is any higher-kinded type.
-- 
-- That is, higher-kinded data are records where all fields in the record are
-- all either of the type @Column f T@ (for any @T@), or are themselves
-- higher-kinded data:
-- 
-- [Nested]
-- 
-- @
-- data Nested f =
--   Nested { nested1 :: MyType f
--          , nested2 :: MyType f
--          }
-- @
-- 
-- The @HigherKindedTable@ type class is used to give us a special mapping
-- operation that lets us change the type parameter @f@.
-- 
-- [Supplying @HigherKindedTable@ instances]
-- 
-- This type class should be derived generically for all table types in your
-- project. To do this, enable the @DeriveAnyType@ and @DeriveGeneric@ language
-- extensions:
-- 
-- @
-- \{\-\# LANGUAGE DeriveAnyClass, DeriveGeneric #-\}
-- 
-- data MyType f = MyType { fieldA :: Column f T }
--   deriving ( GHC.Generics.Generic, HigherKindedTable )
-- @
class HTable (GRep t) => HigherKindedTable (t :: (Type -> Type) -> Type) where
  type GRep t :: (C.Meta -> Type) -> Type
  type GRep t = GColumns (Rep (t Insert))

  toExprs :: t Expr -> GRep t (C.Column Expr)
  fromExprs :: GRep t (C.Column Expr) -> t Expr

  default toExprs
    :: ( GColumns (Rep (t Insert)) ~ GRep t
       , HigherKindedTableImpl Expr (Rep (t Insert)) (Rep (t Expr))
       , Generic (t Expr)
       )
    => t Expr -> GRep t (C.Column Expr)
  toExprs = ghigherKindedTo @Expr @(Rep (t Insert)) . GHC.Generics.from @_ @()

  default fromExprs
    :: ( GColumns (Rep (t Insert)) ~ GRep t
       , HigherKindedTableImpl Expr (Rep (t Insert)) (Rep (t Expr))
       , Generic (t Expr)
       )
    => GRep t (C.Column Expr) -> t Expr
  fromExprs = to @_ @() . ghigherKindedFrom @Expr @(Rep (t Insert))

  toColumnSchemas :: t ColumnSchema -> GRep t (C.Column ColumnSchema)
  fromColumnSchemas :: GRep t (C.Column ColumnSchema) -> t ColumnSchema

  default toColumnSchemas
    :: ( GColumns (Rep (t Insert)) ~ GRep t
       , HigherKindedTableImpl ColumnSchema (Rep (t Insert)) (Rep (t ColumnSchema))
       , Generic (t ColumnSchema)
       )
    => t ColumnSchema -> GRep t (C.Column ColumnSchema)
  toColumnSchemas = ghigherKindedTo @ColumnSchema @(Rep (t Insert)) . GHC.Generics.from @_ @()

  default fromColumnSchemas
    :: ( GColumns (Rep (t Insert)) ~ GRep t
       , HigherKindedTableImpl ColumnSchema (Rep (t Insert)) (Rep (t ColumnSchema))
       , Generic (t ColumnSchema)
       )
    => GRep t (C.Column ColumnSchema) -> t ColumnSchema
  fromColumnSchemas = to @_ @() . ghigherKindedFrom @ColumnSchema @(Rep (t Insert))

  toInserts :: t Insert -> GRep t (C.Column Insert)
  fromInserts :: GRep t (C.Column Insert) -> t Insert

  default toInserts
    :: ( GColumns (Rep (t Insert)) ~ GRep t
       , HigherKindedTableImpl Insert (Rep (t Insert)) (Rep (t Insert))
       , Generic (t Insert)
       )
    => t Insert -> GRep t (C.Column Insert)
  toInserts = ghigherKindedTo @Insert @(Rep (t Insert)) . GHC.Generics.from @_ @()

  default fromInserts
    :: ( GColumns (Rep (t Insert)) ~ GRep t
       , HigherKindedTableImpl Insert (Rep (t Insert)) (Rep (t Insert))
       , Generic (t Insert)
       )
    => GRep t (C.Column Insert) -> t Insert
  fromInserts = to @_ @() . ghigherKindedFrom @Insert @(Rep (t Insert))

  gpack :: GRep t (C.Column Identity) -> t Identity
  gunpack :: t Identity -> GRep t (C.Column Identity)

  default gpack
    :: ( Generic (t Identity)
       , GPack (Rep (t Insert)) (Rep (t Expr)) (Rep (t Identity))
       , GColumns (Rep (t Insert)) ~ GRep t
       )
    => GRep t (C.Column Identity) -> t Identity
  gpack = to @_ @() . gpackImpl @(Rep (t Insert)) @(Rep (t Expr)) @(Rep (t Identity))

  default gunpack
    :: ( Generic (t Identity)
       , GPack (Rep (t Insert)) (Rep (t Expr)) (Rep (t Identity))
       , GColumns (Rep (t Insert)) ~ GRep t
       )
    => t Identity -> GRep t (C.Column Identity)
  gunpack = gunpackImpl @(Rep (t Insert)) @(Rep (t Expr)) @(Rep (t Identity)) . from @_ @()


class GPack (f :: Type -> Type) (g :: Type -> Type) h where
  gpackImpl :: GColumns f (C.Column Identity) -> h x
  gunpackImpl :: h x -> GColumns f (C.Column Identity)


instance GPack f g h => GPack (M1 i a f) (M1 j b g) (M1 k c h) where
  gpackImpl = M1 . gpackImpl @f @g @h
  gunpackImpl (M1 a) = gunpackImpl @f @g @h a


instance (GPack f1 f2 f3, GPack g1 g2 g3) => GPack (f1 :*: g1) (f2 :*: g2) (f3 :*: g3) where
  gpackImpl (HPair x y) = gpackImpl @f1 @f2 @f3 x :*: gpackImpl @g1 @g2 @g3 y
  gunpackImpl (x :*: y) = HPair (gunpackImpl @f1 @f2 @f3 x) (gunpackImpl @g1 @g2 @g3 y)


instance {-# overlaps #-} GPack (K1 i (Maybe (Expr a))) (K1 i (Expr a)) (K1 k a) where
  gpackImpl = K1 . unI . unHIdentity
  gunpackImpl (K1 a) = HIdentity $ C.I a


instance {-# overlaps #-} GPack (K1 i (Expr a)) (K1 j (Expr a)) (K1 k a) where
  gpackImpl = K1 . unI . unHIdentity
  gunpackImpl (K1 a) = HIdentity $ C.I a


instance (Serializable b c, GColumns (K1 i a) ~ Columns b) => GPack (K1 i a) (K1 j b) (K1 k c) where
  gpackImpl = K1 . pack @b
  gunpackImpl (K1 a) = unpack @b a


type family GColumns (shape :: Type -> Type) :: (C.Meta -> Type) -> Type where
  GColumns (M1 _ _ f)              = GColumns f
  GColumns (f :*: g)               = HPair (GColumns f) (GColumns g)
  GColumns (K1 _ (Maybe (Expr a))) = HIdentity ('C.Meta 'C.HasDefault a)
  GColumns (K1 _ (Expr a))         = HIdentity ('C.Meta 'C.NoDefault a)
  GColumns (K1 _ a)                = Columns a


type family GRepAll (f :: Type -> Type) (c :: Type -> Constraint) :: Constraint where
  GRepAll (M1 _ _ f) c            = GRepAll f c
  GRepAll (f :*: g) c             = (GRepAll f c, GRepAll g c)
  GRepAll (K1 _ (Expr a)) c       = c a
  GRepAll (K1 _ a) c              = AllColumns a c


class HigherKindedTableImpl (context :: Type -> Type) (shape :: Type -> Type) (rep :: Type -> Type) where
  ghigherKindedTo :: rep x -> GColumns shape (C.Column context)
  ghigherKindedFrom :: GColumns shape (C.Column context) -> rep x


instance HigherKindedTableImpl context f f' => HigherKindedTableImpl context (M1 i c f) (M1 i' c' f') where
  ghigherKindedTo = ghigherKindedTo @context @f . unM1
  ghigherKindedFrom = M1 . ghigherKindedFrom @context @f


instance (HigherKindedTableImpl context f f', HigherKindedTableImpl context g g') => HigherKindedTableImpl context (f :*: g) (f' :*: g') where
  ghigherKindedTo (x :*: y) = HPair (ghigherKindedTo @context @f x) (ghigherKindedTo @context @g y)
  ghigherKindedFrom (HPair x y) = ghigherKindedFrom @context @f x :*: ghigherKindedFrom @context @g y


instance {-# overlaps #-} HigherKindedTableImpl Insert (K1 i (Maybe (Expr a))) (K1 i (Maybe (Expr a))) where
  ghigherKindedTo = HIdentity . maybe I.InsertDefault I.InsertExpr . unK1
  ghigherKindedFrom = K1 . \case
    HIdentity I.InsertDefault  -> Nothing
    HIdentity (I.InsertExpr a) -> Just a


instance {-# overlaps #-} HigherKindedTableImpl Insert (K1 i' (Expr a)) (K1 i' (Expr a)) where
  ghigherKindedTo = HIdentity . I.InsertExpr . unK1
  ghigherKindedFrom (HIdentity (I.InsertExpr e)) = K1 e


instance (Table Insert a, GColumns (K1 i a) ~ Columns a) => HigherKindedTableImpl Insert (K1 i a) (K1 i a) where
  ghigherKindedTo = toColumns . unK1
  ghigherKindedFrom = K1 . fromColumns


instance {-# overlaps #-} HigherKindedTableImpl Expr (K1 i (Maybe (Expr a))) (K1 i (Expr a)) where
  ghigherKindedTo = HIdentity . E.ExprColumn . unK1
  ghigherKindedFrom = K1 . E.fromExprColumn . unHIdentity


instance {-# overlaps #-} HigherKindedTableImpl Expr (K1 i (Expr a)) (K1 i (Expr a)) where
  ghigherKindedTo = HIdentity . E.ExprColumn . unK1
  ghigherKindedFrom = K1 . E.fromExprColumn . unHIdentity


instance (Table Expr a', GColumns (K1 i a) ~ Columns a') => HigherKindedTableImpl Expr (K1 i a) (K1 i' a') where
  ghigherKindedTo = toColumns . unK1
  ghigherKindedFrom = K1 . fromColumns


instance {-# overlaps #-} HigherKindedTableImpl ColumnSchema (K1 i (Maybe (Expr a))) (K1 i (ColumnSchema a)) where
  ghigherKindedTo = HIdentity . E.ColumnSchemaColumn . unK1
  ghigherKindedFrom = K1 . E.fromColumnSchemaColumn . unHIdentity


instance {-# overlaps #-} HigherKindedTableImpl ColumnSchema (K1 i (Expr a)) (K1 i (ColumnSchema a)) where
  ghigherKindedTo = HIdentity . E.ColumnSchemaColumn . unK1
  ghigherKindedFrom = K1 . E.fromColumnSchemaColumn . unHIdentity


instance (Table ColumnSchema a', GColumns (K1 i a) ~ Columns a') => HigherKindedTableImpl ColumnSchema (K1 i a) (K1 i' a') where
  ghigherKindedTo = toColumns . unK1
  ghigherKindedFrom = K1 . fromColumns


instance (x ~ f, HigherKindedTable t, Helper f t) => Table f (t x) where
  type Columns (t x) = GRep t

  toColumns = helperTo
  fromColumns = helperFrom


class Helper f t where
  helperTo :: t f -> GRep t (C.Column f)
  helperFrom :: GRep t (C.Column f) -> t f


instance HigherKindedTable t => Helper Expr t where
  helperTo = toExprs
  helperFrom = fromExprs


instance HigherKindedTable t => Helper ColumnSchema t where
  helperTo = toColumnSchemas
  helperFrom = fromColumnSchemas


instance HigherKindedTable t => Helper Insert t where
  helperTo = toInserts
  helperFrom = fromInserts


instance (HigherKindedTable t, s ~ t, columnSchema ~ ColumnSchema, expr ~ Expr) => Selects (s columnSchema) (t expr)


instance (s ~ t, t ~ u, HigherKindedTable t, columnSchema ~ ColumnSchema, insert ~ Insert, expr ~ Expr) => Inserts (s columnSchema) (s insert) (u expr)


instance (HigherKindedTable t, a ~ t Expr, identity ~ Identity) => ExprFor a (t identity) where
  pack = gpack
  unpack = gunpack

instance (s ~ t, expr ~ Expr, identity ~ Identity, HigherKindedTable t) => Serializable (s expr) (t identity) where
