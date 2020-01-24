{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Column
  ( Column
  , C( MkC, toColumn )
  , mapC
  , mapCC
  , castC
  , sequenceC
  , traverseC
  , traverseCC
  , zipCWithM
  , zipCWithMC
  , Null
  , Id
  , Select
  , From
  , Demote
  , Structure
  , Lit
  ) where

import Data.Functor.Identity
import Data.Kind


data Null ( f :: * -> * ) a


data Id ( f :: * -> * ) a


data From ( m :: * -> * ) ( f :: * -> * ) a


data Select ( f :: * -> * ) a


data Demote ( f :: * -> * ) a


data Structure ( f :: * -> * ) a


data Lit ( f :: * -> * ) a


{-| The @Column@ type family should be used to indicate which fields of your
data types are single columns in queries. This type family has special
support when a query is executed, allowing you to use a single data type for
both query data and rows decoded to Haskell.

To understand why this type family is special, let's consider a simple
higher-kinded data type of Haskell packages:

@
data HaskellPackage f =
  HaskellPackage
    { packageName :: Column f String
    , packageAuthor :: Column f String
    }
@

In queries, @f@ will be some type of 'Rel8.Expr.Expr', and @Column ( Expr .. )@
reduces to just @Expr ..@:

>>> :t packageName ( package :: Package ( Expr m ) )
Expr m String

When we 'Rel8.Query.select' queries of this type, @f@ will be instantiated as
@Identity@, at which point all wrapping entire disappears:

>>> :t packageName ( package :: Package Identity )
String

In @rel8@ we try hard to always know roughly what @f@ is, which means typed
holes should mention precise types, rather than the @Column@ type family. You
should only need to be aware of the type family when defining your table types.
-}
type family Column ( context :: Type -> Type ) ( a :: Type ) :: Type where
  Column ( Null f ) a = Column f ( Maybe ( DropMaybe a ) )
  Column Identity a = a
  Column f a = f a


type family DropMaybe ( a :: Type ) :: Type where
  DropMaybe ( Maybe a ) = DropMaybe a
  DropMaybe a = a


-- | The @C@ newtype simply wraps 'Column', but this allows us to work
-- injectivity problems of functions that return type family applications
-- (for example, 'Rel8.HigherKinded.zipRecord').
newtype C f x =
  MkC { toColumn :: Column f x }


-- | Map from one column to another.
mapC :: ( Column f x -> Column g y ) -> C f x -> C g y
mapC f ( MkC x ) =
  MkC ( f x )


-- | Map from one column to another, where columns types are subject to a
-- constraint.
mapCC :: forall c f g x y. c x => ( c x => Column f x -> Column g y ) -> C f x -> C g y
mapCC f ( MkC x ) =
  MkC ( f x )


-- | Effectfully map from one column to another.
traverseC
  :: Applicative m
  => ( Column f x -> m ( Column g y ) ) -> C f x -> m ( C g y )
traverseC f ( MkC x ) =
  MkC <$> f x


-- | Effectfully map from one column to another, where column types are subject
-- to a constraint.
traverseCC
  :: forall c x y f g m
   . ( Applicative m, c x, c y )
  => ( ( c x, c y ) => Column f x -> m ( Column g y ) )
  -> C f x
  -> m ( C g y )
traverseCC f ( MkC x ) =
  MkC <$> f x


-- | If you know two columns are actually equal, @castC@ lets you cast between
-- them.
castC :: Column f a ~ Column g b => C f a -> C g b
castC ( MkC x ) =
  MkC x


-- | If a column contains an effectful operation, sequence that operation into a
--  new column.
sequenceC
  :: ( Column f a ~ m ( Column g y ), Functor m )
  => C f a -> m ( C g y )
sequenceC ( MkC x ) =
  MkC <$> x


-- | Zip two columns together under an effectful context.
zipCWithM
  :: Applicative m
  => ( Column f x -> Column g y -> m ( Column h z ) )
  -> C f x -> C g y -> m ( C h z )
zipCWithM f ( MkC x ) ( MkC y ) =
  MkC <$> f x y


-- | Zip two columns together under an effectful context, where all column types
-- satisfy a common constraint.
zipCWithMC
  :: forall c f g h x y z m
   . ( Applicative m, c x, c y, c z )
  => ( ( c x, c y, c z ) => Column f x -> Column g y -> m ( Column h z ) )
  -> C f x -> C g y -> m ( C h z )
zipCWithMC f ( MkC x ) ( MkC y ) =
  MkC <$> f x y
