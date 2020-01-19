{-# language DataKinds #-}
{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Table where

import Data.Functor.Identity
import Data.Monoid
import Data.Proxy
import GHC.Exts ( Constraint )
import GHC.Generics ( Rep, M1(..), (:*:)(..), K1(..), Generic, to, from )
import Rel8.Column
import Rel8.Unconstrained


-- | The class of Haskell types that represent SQL tables.
class ConstrainTable t Unconstrained => Table ( t :: * ) where
  -- | The @Field@ type is a type where each value corresponds to a distinct
  -- field in the table.
  type Field t = ( field :: * -> * ) | field -> t

  -- | All fields in a table are intepreted under a common functor @f@.
  -- This associated type family lets us extract that functor.
  type Context t :: * -> *

  -- | Give the tag of field in the table, look at the contents of that field.
  field :: t -> Field t x -> C ( Context t ) x

  -- | Given a function that knows how to construct fields in the 'Context' of
  -- the table @t@, build a @t@ by calling that function for every field.
  tabulateMCP
    :: forall c f proxy
     . ( ConstrainTable t c, Applicative f )
    => proxy c
    -> ( forall x. c x => Field t x -> f ( C ( Context t ) x ) )
    -> f t

  type ConstrainTable t ( c :: * -> Constraint ) :: Constraint


class ( Table a, Table b, Context a ~ f, Context b ~ g ) => Compatible a ( f :: * -> * ) b ( g :: * -> * ) | a -> f, b -> g, f b g -> a where
  transferField :: Field a x -> Field b x


tabulateC
  :: forall c t. ( Table t, ConstrainTable t c )
  => ( forall x. c x => Field t x -> C ( Context t ) x )
  -> t
tabulateC f =
  runIdentity ( tabulateMCP ( Proxy @c ) ( Identity . f ) )


-- | Effectfully map a table from one context to another.
traverseTableWithIndexC
  :: forall c t t' f g m
   . ( Applicative m, Compatible t' f t g, ConstrainTable t' c )
  => ( forall x. c x => Field t x -> C ( Context t ) x -> m ( C ( Context t' ) x ) )
  -> t
  -> m t'
traverseTableWithIndexC f t =
  tabulateMCP ( Proxy @c ) \index ->
    f ( transferField index ) ( field t ( transferField index ) )


data TupleField a b x where
  Element1 :: Field a x -> TupleField a b x
  Element2 :: Field b x -> TupleField a b x


instance ( Context a ~ Context b, Table a, Table b ) => Table ( a, b ) where
  type Context ( a, b ) =
    Context a

  type ConstrainTable ( a, b ) c =
    ( ConstrainTable a c, ConstrainTable b c )

  type Field ( a, b ) = TupleField a b

  field ( a, b ) = \case
    Element1 f -> field a f
    Element2 f -> field b f

  tabulateMCP proxy f =
    (,) <$> tabulateMCP proxy ( f . Element1 )
        <*> tabulateMCP proxy ( f . Element2 )


instance ( Context x ~ Context y, Context a ~ Context b, Compatible a f x g, Compatible b f y g ) => Compatible ( a, b ) f ( x, y ) g where
  transferField = \case
    Element1 i -> Element1 ( transferField i )
    Element2 i -> Element2 ( transferField i )


data SumField a x where
  SumField :: Field a x -> SumField a x


instance Table a => Table ( Sum a ) where
  type ConstrainTable ( Sum a ) c =
    ConstrainTable a c

  type Context ( Sum a ) =
    Context a

  type Field ( Sum a ) = SumField a

  field ( Sum a ) ( SumField i ) =
    field a i

  tabulateMCP proxy f =
    Sum <$> tabulateMCP proxy ( f . SumField )


instance Compatible a f b g => Compatible ( Sum a ) f ( Sum b ) g where
  transferField ( SumField x ) =
    SumField ( transferField x )


tabulate :: Table t => ( forall x. Field t x -> C ( Context t ) x ) -> t
tabulate f =
  tabulateC @Unconstrained f


mapTable
  :: forall t' t
   . ( Compatible t' ( Context t' ) t ( Context t ) )
  => ( forall x. C ( Context t ) x -> C ( Context t' ) x ) -> t -> t'
mapTable f =
  runIdentity . traverseTable ( Identity . f )


traverseTable
  :: ( Applicative f, Compatible t' ( Context t' ) t ( Context t ) )
  => ( forall x. C ( Context t ) x -> f ( C ( Context t' ) x ) )
  -> t
  -> f t'
traverseTable f =
  traverseTableWithIndexC @Unconstrained ( const f )


traverseTableC
  :: forall c m t t'
   . ( Applicative m, ConstrainTable t' c, Compatible t' ( Context t' ) t ( Context t ) )
  => ( forall x. c x => C ( Context t ) x -> m ( C ( Context t' ) x ) )
  -> t
  -> m t'
traverseTableC f =
  traverseTableWithIndexC @c ( const f )


zipTablesWithM
  :: forall t t' t'' m
   . ( Applicative m
     , Table t''
     , Compatible t'' ( Context t'' ) t ( Context t )
     , Compatible t'' ( Context t'' ) t' ( Context t' )
     )
  => ( forall x. C ( Context t ) x -> C ( Context t' ) x -> m ( C ( Context t'' ) x ) )
  -> t -> t' -> m t''
zipTablesWithM f t t' =
  tabulateMCP @t'' ( Proxy @Unconstrained ) \index ->
    f ( field t ( transferField index ) )
      ( field t' ( transferField index ) )


zipTablesWithMC
  :: forall c t'' t t' m
   . ( ConstrainTable t'' c
     , Compatible t'' ( Context t'' ) t ( Context t )
     , Compatible t'' ( Context t'' ) t' ( Context t' )
     , Applicative m
     )
  => ( forall x. c x => C ( Context t ) x -> C ( Context t' ) x -> m ( C ( Context t'' ) x ) )
  -> t -> t' -> m t''
zipTablesWithMC f t t' =
  tabulateMCP @t'' ( Proxy @c) \index ->
    f ( field t ( transferField index ) ) ( field t' ( transferField index ) )


data GenericField t a where
  GenericField :: GHField t ( Rep ( t Spine ) ) a -> GenericField t a


class HigherKindedTable ( t :: ( * -> * ) -> * ) where
  type HField t = ( field :: * -> * ) | field -> t
  type HField t =
    GenericField t

  type HConstrainTraverse t ( f :: * -> * ) ( c :: * -> Constraint ) :: Constraint
  type HConstrainTraverse t f c =
    GHConstrainTraverse ( Rep ( t f ) ) ( Rep ( t Spine ) ) c

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

  htabulate
    :: ( Applicative m, HConstrainTraverse t f c )
    => proxy c -> ( forall x. c x => HField t x -> m ( C f x ) ) -> m ( t f )

  default htabulate
    :: forall f m c proxy
     . ( Applicative m, GHConstrainTraverse ( Rep ( t f ) ) ( Rep ( t Spine ) ) c, Generic ( t f )
       , GHigherKindedTable ( Rep ( t f ) ) t f ( Rep ( t Spine ) )
       , HField t ~ GenericField t
       )
    => proxy c -> ( forall x. c x => HField t x -> m ( C f x ) ) -> m ( t f )
  htabulate proxy f =
    fmap to ( ghtabulate @( Rep ( t f ) ) @t @f @( Rep ( t Spine ) ) proxy ( f . GenericField ) )



data HFieldField t ( f :: * -> * ) x where
  F :: HField t x -> HFieldField t f x


instance ( ConstrainTable ( t f ) Unconstrained, HigherKindedTable t ) => Table ( t f ) where
  type Field ( t f ) =
    HFieldField t f

  type Context ( t f ) =
    f

  type ConstrainTable ( t f ) c =
    HConstrainTraverse t f c

  tabulateMCP proxy f =
    htabulate proxy \x -> f ( F x )

  field x ( F i ) =
    hfield x i


instance ( HConstrainTraverse t' g Unconstrained, HConstrainTraverse t' f Unconstrained, HConstrainTraverse t f Unconstrained, HigherKindedTable t, t ~ t', f ~ f', g ~ g' ) => Compatible ( t f ) f' ( t' g ) g' where
  transferField ( F x ) =
    F x


class GHigherKindedTable ( rep :: * -> * ) ( t :: ( * -> * ) -> * ) ( f :: * -> * ) ( repIdentity :: * -> * ) where
  data GHField t repIdentity :: * -> *

  type GHConstrainTraverse rep repIdentity ( c :: * -> Constraint ) :: Constraint

  ghfield :: rep a -> GHField t repIdentity x -> C f x

  ghtabulate
    :: ( Applicative m, GHConstrainTraverse rep repIdentity c )
    => proxy c
    -> ( forall x. c x => GHField t repIdentity x -> m ( C f x ) )
    -> m ( rep a )


instance GHigherKindedTable x t f x' => GHigherKindedTable ( M1 i c x ) t f ( M1 i' c' x' ) where
  data GHField t ( M1 i' c' x' ) a where
    M1Field :: GHField t x' a -> GHField t ( M1 i' c' x' ) a

  type GHConstrainTraverse ( M1 i c x ) ( M1 i' c' x' ) constraint =
    GHConstrainTraverse x x' constraint

  ghfield ( M1 a ) ( M1Field i ) =
    ghfield a i

  ghtabulate proxy f =
    M1 <$> ghtabulate @x @t @f @x' proxy ( f . M1Field )


instance ( GHigherKindedTable x t f x', GHigherKindedTable y t f y' ) => GHigherKindedTable ( x :*: y ) t f ( x' :*: y' ) where
  data GHField t ( x' :*: y' ) a where
    FieldL :: GHField t x' a -> GHField t ( x' :*: y' ) a
    FieldR :: GHField t y' a -> GHField t ( x' :*: y' ) a

  type GHConstrainTraverse ( x :*: y ) ( x' :*: y' ) constraint =
    ( GHConstrainTraverse x x' constraint, GHConstrainTraverse y y' constraint )

  ghfield ( x :*: y ) = \case
    FieldL i -> ghfield x i
    FieldR i -> ghfield y i

  ghtabulate proxy f =
    (:*:) <$> ghtabulate @x @t @f @x' proxy ( f . FieldL )
          <*> ghtabulate @y @t @f @y' proxy ( f . FieldR )


type family IsColumnApplication ( a :: * ) :: Bool where
  IsColumnApplication ( Spine a ) = 'True
  IsColumnApplication _ = 'False



instance DispatchK1 ( IsColumnApplication c' ) f c c' => GHigherKindedTable ( K1 i c ) t f ( K1 i' c' ) where
  data GHField t ( K1 i' c' ) a where
    K1Field :: K1Field ( IsColumnApplication c' ) c' x -> GHField t ( K1 i' c' ) x

  type GHConstrainTraverse ( K1 i c ) ( K1 i' c' ) constraint =
    ConstrainK1 ( IsColumnApplication c' ) c c' constraint

  ghfield ( K1 a ) ( K1Field i ) =
    k1field @( IsColumnApplication c' ) @f @c @c' a i

  ghtabulate proxy f =
    K1 <$> k1tabulate @( IsColumnApplication c' ) @f @c @c' proxy ( f . K1Field )


class DispatchK1 ( isSpine :: Bool ) f a a' where
  data K1Field isSpine a' :: * -> *

  type ConstrainK1 isSpine a a' ( c :: * -> Constraint ) :: Constraint

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
    C a

  k1tabulate _ f =
    toColumn <$> f @b K1True


instance ( Context a ~ f, Table a, Compatible a' Spine a f, Compatible a f a' Spine ) => DispatchK1 'False f a a' where
  data K1Field 'False a' x where
    K1False :: Field a' x -> K1Field 'False a' x

  type ConstrainK1 'False a a' c =
    ConstrainTable a c

  k1field a ( K1False i ) =
    field a ( transferField i )

  k1tabulate proxy f =
    tabulateMCP proxy ( f . K1False . transferField )
