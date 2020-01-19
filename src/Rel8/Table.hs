{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
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
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Table where

import Data.Functor.Identity
import Data.Monoid
import Data.Proxy
import GHC.Exts ( Constraint )
import Rel8.Column
import Rel8.Unconstrained


-- | The class of Haskell types that represent SQL tables.
class ConstrainTable t Unconstrained => Table ( t :: * ) where
  -- | The @Field@ type is a type where each value corresponds to a distinct
  -- field in the table.
  data Field t :: * -> *

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


instance ( Context a ~ Context b, Table a, Table b ) => Table ( a, b ) where
  type Context ( a, b ) =
    Context a

  type ConstrainTable ( a, b ) c =
    ( ConstrainTable a c, ConstrainTable b c )

  data Field ( a, b ) x
    = Element1 ( Field a x )
    | Element2 ( Field b x )

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


instance Table a => Table ( Sum a ) where
  type ConstrainTable ( Sum a ) c =
    ConstrainTable a c

  type Context ( Sum a ) =
    Context a

  data Field ( Sum a ) x =
    SumField ( Field a x )

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


class HigherKindedTable ( t :: ( * -> * ) -> * ) where
  data HField t :: * -> *

  hfield :: t f -> HField t x -> C f x

  htabulate
    :: ( Applicative m, HConstrainTraverse t c )
    => proxy c -> ( forall x. c x => HField t x -> m ( C f x ) ) -> m ( t f )

  type HConstrainTraverse t ( c :: * -> Constraint ) :: Constraint


instance ( ConstrainTable ( t f ) Unconstrained, HigherKindedTable t ) => Table ( t f ) where
  data Field ( t f ) x where
    F :: HField t x -> Field ( t f ) x

  type Context ( t f ) =
    f

  type ConstrainTable ( t f ) c =
    HConstrainTraverse t c

  tabulateMCP proxy f =
    htabulate proxy \x -> f ( F x )

  field x ( F i ) =
    hfield x i


instance ( HConstrainTraverse t Unconstrained, HigherKindedTable t, t ~ t', f ~ f', g ~ g' ) => Compatible ( t f ) f' ( t' g ) g' where
  transferField ( F x ) =
    F x
