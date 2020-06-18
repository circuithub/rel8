{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

{-| Every relational library also tries to build some type of record system,
and Rel8 is no exception! We've tried hard to make sure you don't have to
understand this module to enjoy using Rel8 - in fact, you can probably close
your browser tab now if you want to! However, this is the API that's used
behind the scenes, and is safely exported if you want to use it in your own
work, or if you want to understand further how Rel8 works.

-}
module Rel8.Table where

import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import GHC.Generics hiding ( C )
import Rel8.Column
import Rel8.Unconstrained


{-| Higher-kinded data types.

Higher-kinded data types are data types of the pattern:

@
data MyType f =
  MyType { field1 :: Column f T1 OR HK1 f
         , field2 :: Column f T2 OR HK2 f
         , ...
         , fieldN :: Column f Tn OR HKn f
         }
@

where @Tn@ is any Haskell type, and @HKn@ is any higher-kinded type.

That is, higher-kinded data are records where all fields in the record
are all either of the type @Column f T@ (for any @T@), or are themselves
higher-kinded data:

[Nested]

@
data Nested f =
  Nested { nested1 :: MyType f
         , nested2 :: MyType f
         }
@

The @HigherKindedTable@ type class is used to give us a special mapping
operation that lets us change the type parameter @f@.

[Supplying @HigherKindedTable@ instances]

This type class should be derived generically for all table types in your
project. To do this, enable the @DeriveAnyType@ and @DeriveGeneric@ language
extensions:

@
\{\-\# LANGUAGE DeriveAnyClass, DeriveGeneric #-\}
import qualified GHC.Generics

data MyType f = MyType { fieldA :: Column f T }
  deriving ( GHC.Generics.Generic, HigherKindedTable )
@

-}
class HigherKindedTable ( t :: ( Type -> Type ) -> Type ) where
  -- | Like 'Field', but for higher-kinded tables.
  type HField t = ( field :: Type -> Type ) | field -> t
  type HField t =
    GenericField t

  -- | Like 'Constraintable', but for higher-kinded tables.
  type HConstrainTable t ( f :: Type -> Type ) ( c :: Type -> Constraint ) :: Constraint
  type HConstrainTable t f c =
    GHConstrainTable ( Rep ( t f ) ) ( Rep ( t SPINE ) ) c

  -- | Like 'field', but for higher-kinded tables.
  hfield :: t f -> HField t x -> C f x
  default hfield
    :: forall f x
     . ( Generic ( t f )
       , HField t ~ GenericField t
       , GHigherKindedTable ( Rep ( t f ) ) t f ( Rep ( t SPINE ) )
       )
    => t f -> HField t x -> C f x
  hfield x ( GenericField i ) =
    ghfield @( Rep ( t f ) ) @t @f @( Rep ( t SPINE ) ) ( from x ) i

  -- | Like 'tabulateMCP', but for higher-kinded tables.
  htabulate
    :: ( Applicative m, HConstrainTable t f c )
    => proxy c -> ( forall x. c x => HField t x -> m ( C f x ) ) -> m ( t f )

  default htabulate
    :: forall f m c proxy
     . ( Applicative m, GHConstrainTable ( Rep ( t f ) ) ( Rep ( t SPINE ) ) c, Generic ( t f )
       , GHigherKindedTable ( Rep ( t f ) ) t f ( Rep ( t SPINE ) )
       , HField t ~ GenericField t
       )
    => proxy c -> ( forall x. c x => HField t x -> m ( C f x ) ) -> m ( t f )
  htabulate proxy f =
    fmap to ( ghtabulate @( Rep ( t f ) ) @t @f @( Rep ( t SPINE ) ) proxy ( f . GenericField ) )



data TableHField t ( f :: Type -> Type ) x where
  F :: HField t x -> TableHField t f x


-- | Any 'HigherKindedTable' is also a 'Table'.
instance HigherKindedTable t => Table (t f) where
  type Structure (t f) = t
  type Context (t f) = f
  toStructure = id
  fromStructure = id


data GenericField t a where
  GenericField :: GHField t ( Rep ( t SPINE ) ) a -> GenericField t a


class GHigherKindedTable ( rep :: Type -> Type ) ( t :: ( Type -> Type ) -> Type ) ( f :: Type -> Type ) ( repIdentity :: Type -> Type ) where
  data GHField t repIdentity :: Type -> Type

  type GHConstrainTable rep repIdentity ( c :: Type -> Constraint ) :: Constraint

  ghfield :: rep a -> GHField t repIdentity x -> C f x

  ghtabulate
    :: ( Applicative m, GHConstrainTable rep repIdentity c )
    => proxy c
    -> ( forall x. c x => GHField t repIdentity x -> m ( C f x ) )
    -> m ( rep a )


instance GHigherKindedTable x t f x' => GHigherKindedTable ( M1 i c x ) t f ( M1 i' c' x' ) where
  data GHField t ( M1 i' c' x' ) a where
    M1Field :: GHField t x' a -> GHField t ( M1 i' c' x' ) a

  type GHConstrainTable ( M1 i c x ) ( M1 i' c' x' ) constraint =
    GHConstrainTable x x' constraint

  ghfield ( M1 a ) ( M1Field i ) =
    ghfield a i

  ghtabulate proxy f =
    M1 <$> ghtabulate @x @t @f @x' proxy ( f . M1Field )


instance ( GHigherKindedTable x t f x', GHigherKindedTable y t f y' ) => GHigherKindedTable ( x :*: y ) t f ( x' :*: y' ) where
  data GHField t ( x' :*: y' ) a where
    FieldL :: GHField t x' a -> GHField t ( x' :*: y' ) a
    FieldR :: GHField t y' a -> GHField t ( x' :*: y' ) a

  type GHConstrainTable ( x :*: y ) ( x' :*: y' ) constraint =
    ( GHConstrainTable x x' constraint, GHConstrainTable y y' constraint )

  ghfield ( x :*: y ) = \case
    FieldL i -> ghfield x i
    FieldR i -> ghfield y i

  ghtabulate proxy f =
    (:*:) <$> ghtabulate @x @t @f @x' proxy ( f . FieldL )
          <*> ghtabulate @y @t @f @y' proxy ( f . FieldR )


type family IsColumnApplication ( a :: Type ) :: Bool where
  IsColumnApplication ( SPINE a ) = 'True
  IsColumnApplication _ = 'False



instance DispatchK1 ( IsColumnApplication c' ) f c c' => GHigherKindedTable ( K1 i c ) t f ( K1 i' c' ) where
  data GHField t ( K1 i' c' ) a where
    K1Field :: K1Field ( IsColumnApplication c' ) c' x -> GHField t ( K1 i' c' ) x

  type GHConstrainTable ( K1 i c ) ( K1 i' c' ) constraint =
    ConstrainK1 ( IsColumnApplication c' ) c c' constraint

  ghfield ( K1 a ) ( K1Field i ) =
    k1field @( IsColumnApplication c' ) @f @c @c' a i

  ghtabulate proxy f =
    K1 <$> k1tabulate @( IsColumnApplication c' ) @f @c @c' proxy ( f . K1Field )


class DispatchK1 ( isSPINE :: Bool ) f a a' where
  data K1Field isSPINE a' :: Type -> Type

  type ConstrainK1 isSPINE a a' ( c :: Type -> Constraint ) :: Constraint

  k1field :: a -> K1Field isSPINE a' x -> C f x

  k1tabulate
    :: ( ConstrainK1 isSPINE a a' c, Applicative m )
    => proxy c -> ( forall x. c x => K1Field isSPINE a' x -> m ( C f x ) ) -> m a


instance (a ~ Column f b) => DispatchK1 'True f a ( SPINE b ) where
  data K1Field 'True ( SPINE b ) x where
    K1True :: K1Field 'True ( SPINE b ) b

  type ConstrainK1 'True a ( SPINE b ) c =
    c b

  k1field a K1True =
    MkC a

  k1tabulate _ f =
    toColumn <$> f @b K1True


data SPINE a


{-| Types that represent SQL tables.

You generally should not need to derive instances of this class manually, as
writing higher-kinded data types is usually more convenient. See also:
'HigherKindedTable'.

-}
class HigherKindedTable (Structure t) => Table (t :: Type) where
  type Structure t :: (Type -> Type) -> Type
  type Context t :: Type -> Type

  toStructure :: t -> Structure t (Context t)
  fromStructure :: Structure t (Context t) -> t



-- | Effectfully map a table from one context to another.
traverseTableWithIndexC
  :: forall c t t' m
   . (Applicative m, Table t, Table t', HConstrainTable (Structure t') (Context t') c, Structure t ~ Structure t')
  => (forall x. c x => HField (Structure t) x -> C (Context t) x -> m (C (Context t') x))
  -> t
  -> m t'
traverseTableWithIndexC f t =
  fmap fromStructure $
  htabulate (Proxy @c) \index -> f index (hfield (toStructure t) index)


data HPair x y (f :: Type -> Type) = HPair { hfst :: x f, hsnd :: y f }
  deriving (Generic)


deriving instance (HigherKindedTable x, HigherKindedTable y) => HigherKindedTable (HPair x y)


instance (Table a, Table b, Context a ~ Context b) => Table (a, b) where
  type Structure (a, b) = HPair (Structure a) (Structure b)
  type Context (a, b) = Context a
  toStructure (a, b) = HPair (toStructure a) (toStructure b)
  fromStructure (HPair x y) = (fromStructure x, fromStructure y)


-- | Map a 'Table' from one type to another. The table types must be compatible,
-- see 'Compatible' for what that means.
mapTable
  :: forall t' t
   . (Table t, Table t', Structure t ~ Structure t', HConstrainTable (Structure t) (Context t') Unconstrained)
  => (forall x. C (Context t) x -> C (Context t') x) -> t -> t'
mapTable f = runIdentity . traverseTable (Identity . f)


-- | Map a 'Table' from one type to another, where all columns in the table are
-- subject to a constraint.
mapTableC
  :: forall c t' t
   . (Table t, Table t', Structure t ~ Structure t', HConstrainTable (Structure t) (Context t') c)
  => (forall x. c x => C (Context t) x -> C (Context t') x)
  -> t -> t'
mapTableC f =
  runIdentity . traverseTableC @c ( Identity . f )


-- | Effectfully traverse all fields in a 'Table', potentially producing another
-- @Table@.
traverseTable
  :: forall t' t m
   . (Applicative m, Table t, Table t', Structure t ~ Structure t', HConstrainTable (Structure t) (Context t') Unconstrained)
  => (forall x. C (Context t) x -> m (C (Context t') x))
  -> t
  -> m t'
traverseTable f =
  traverseTableWithIndexC @Unconstrained (const f)


-- | Effectfully traverse all fields in a 'Table', provided that all fields
-- satisfy a given constraint. For example, if all fields in a table have an
-- instance for 'Read', we can apply 'readMaybe' to all fields in the table,
-- failing if any read fails:
--
-- >>> traverseTableC @Read ( traverseC readMaybe ) MyTable{ fieldA = "True" }
-- Just MyTable{ fieldA = True }
traverseTableC
  :: forall c m t t'
   . (Table t, Table t', Applicative m, Structure t ~ Structure t', HConstrainTable (Structure t) (Context t') c)
  => (forall x. c x => C (Context t) x -> m (C (Context t') x))
  -> t
  -> m t'
traverseTableC f =
  traverseTableWithIndexC @c (const f)


zipTablesWithM
  :: forall t m
   . ( Applicative m
     , Table t
     , HConstrainTable (Structure t) (Context t) Unconstrained
     )
  => ( forall x. C ( Context t ) x -> C ( Context t ) x -> m ( C ( Context t ) x ) )
  -> t -> t -> m t
zipTablesWithM f t t' =
  fmap fromStructure $
  htabulate (Proxy @Unconstrained) \index ->
    f (hfield (toStructure t) index) (hfield (toStructure t') index)


instance (Context a ~ f, Table a, Structure a ~ Structure a') => DispatchK1 'False f a a' where
  data K1Field 'False a' x where
    K1False :: HField (Structure a') x -> K1Field 'False a' x

  type ConstrainK1 'False a a' c =
    HConstrainTable (Structure a) (Context a) c

  k1field a (K1False i) =
    hfield (toStructure a) i

  k1tabulate proxy f =
    fromStructure <$> htabulate proxy (f . K1False)


newtype HIdentity a f = HIdentity { unHIdentity :: Column f a }
  deriving ( Generic, HigherKindedTable )


-- | Any 'Expr' can be seen as a 'Table' with only one column.
instance Table (Identity a) where
  type Context (Identity a) = Identity
  type Structure (Identity a) = HIdentity a
  toStructure = HIdentity . runIdentity
  fromStructure = Identity . unHIdentity
