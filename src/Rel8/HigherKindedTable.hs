{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-# options -fno-warn-orphans #-}

module Rel8.HigherKindedTable ( HigherKindedTable(..) ) where

import Data.Functor.Identity
import Data.Kind
import GHC.Generics hiding ( C )
import Rel8.Column
import Rel8.Expr
import Rel8.Table


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
    GHConstrainTable ( Rep ( t f ) ) ( Rep ( t Spine ) ) c

  -- | Like 'field', but for higher-kinded tables.
  hfield :: t f -> HField t x -> C f x
  default hfield
    :: forall f x
     . ( Generic ( t f )
       , HField t ~ GenericField t
       , GHigherKindedTable ( Rep ( t f ) ) t f ( Rep ( t Spine ) )
       )
    => t f -> HField t x -> C f x
  hfield x ( GenericField i ) =
    ghfield @( Rep ( t f ) ) @t @f @( Rep ( t Spine ) ) ( from x ) i

  -- | Like 'tabulateMCP', but for higher-kinded tables.
  htabulate
    :: ( Applicative m, HConstrainTable t f c )
    => proxy c -> ( forall x. c x => HField t x -> m ( C f x ) ) -> m ( t f )

  default htabulate
    :: forall f m c proxy
     . ( Applicative m, GHConstrainTable ( Rep ( t f ) ) ( Rep ( t Spine ) ) c, Generic ( t f )
       , GHigherKindedTable ( Rep ( t f ) ) t f ( Rep ( t Spine ) )
       , HField t ~ GenericField t
       )
    => proxy c -> ( forall x. c x => HField t x -> m ( C f x ) ) -> m ( t f )
  htabulate proxy f =
    fmap to ( ghtabulate @( Rep ( t f ) ) @t @f @( Rep ( t Spine ) ) proxy ( f . GenericField ) )



data TableHField t ( f :: Type -> Type ) x where
  F :: HField t x -> TableHField t f x


-- | Any 'HigherKindedTable' is also a 'Table'.
instance ( ConstrainTable ( t f ) Unconstrained, HigherKindedTable t ) => Table ( t f ) where
  type Field ( t f ) =
    TableHField t f

  type Context ( t f ) =
    f

  type ConstrainTable ( t f ) c =
    HConstrainTable t f c

  tabulateMCP proxy f =
    htabulate proxy \x -> f ( F x )

  field x ( F i ) =
    hfield x i


type family Reduce ( f :: * -> * ) :: ( * -> * ) where
  Reduce ( Id x ) = x
  Reduce ( Select Expr ) = Identity
  Reduce Expr = Expr
  Reduce ( Structure f ) = Spine
  Reduce ( From f ) = Expr
  Reduce ( Lit Identity ) = Expr


instance ( HigherKindedTable t, HConstrainTable t ( Reduce ( g f ) ) Unconstrained, HConstrainTable t f Unconstrained ) => Recontextualise ( t f ) g where
  type MapTable g ( t f ) = t ( Reduce ( g f ) )
  fieldMapping ( F i ) = F i
  reverseFieldMapping ( F i ) = F i


data GenericField t a where
  GenericField :: GHField t ( Rep ( t Spine ) ) a -> GenericField t a


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
  IsColumnApplication ( Spine a ) = 'True
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


class DispatchK1 ( isSpine :: Bool ) f a a' where
  data K1Field isSpine a' :: Type -> Type

  type ConstrainK1 isSpine a a' ( c :: Type -> Constraint ) :: Constraint

  k1field :: a -> K1Field isSpine a' x -> C f x

  k1tabulate
    :: ( ConstrainK1 isSpine a a' c, Applicative m )
    => proxy c -> ( forall x. c x => K1Field isSpine a' x -> m ( C f x ) ) -> m a


instance a ~ Column f b => DispatchK1 'True f a ( Spine b ) where
  data K1Field 'True ( Spine b ) x where
    K1True :: K1Field 'True ( Spine b ) b

  type ConstrainK1 'True a ( Spine b ) c =
    c b

  k1field a K1True =
    MkC a

  k1tabulate _ f =
    toColumn <$> f @b K1True


instance ( Context a ~ f, Table a, Field a' ~ Field ( MapTable Structure a ), Recontextualise a Structure ) => DispatchK1 'False f a a' where
  data K1Field 'False a' x where
    K1False :: Field a' x -> K1Field 'False a' x

  type ConstrainK1 'False a a' c =
    ConstrainTable a c

  k1field a ( K1False i ) =
    field a ( fieldMapping @_ @Structure i )

  k1tabulate proxy f =
    tabulateMCP proxy ( f . K1False . reverseFieldMapping @_ @Structure )


data Spine a
