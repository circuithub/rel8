{-# language AllowAmbiguousTypes #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Column
  ( Column
  , MkColumn
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
  ) where

import Data.Functor.Identity
import Data.Kind
import Data.Void
import Rel8.ColumnSchema
import Rel8.Context
import Rel8.Null
import Rel8.Shape
import {-# source #-} Rel8.Expr


type family MkColumn ( context :: Context ) ( a :: Null Type ) :: Type where
  MkColumn ( 'SQL m ) a = Expr m a
  MkColumn 'Haskell ( 'Null a ) = Maybe a
  MkColumn 'Haskell ( 'NotNull a ) = a
  MkColumn ( 'ToNull ( SQL m ) ) ( _ a ) = Expr m ( 'Null a )
  MkColumn ( 'ToNull Haskell ) ( _ a ) = Maybe a
  MkColumn 'Shape a = Shape a
  MkColumn 'Schema a = ColumnSchema a


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
type family Column ( context :: Context ) ( a :: Type ) where
  Column context ( Maybe a ) = MkColumn context ( 'Null a )
  Column context a = MkColumn context ( 'NotNull a )


-- | The @C@ newtype simply wraps 'Column', but this allows us to work
-- injectivity problems of functions that return type family applications
-- (for example, 'Rel8.HigherKinded.zipRecord').
newtype C f x =
  MkC { toColumn :: MkColumn f x }


-- | Map from one column to another.
mapC :: ( MkColumn f x -> MkColumn g y ) -> C f x -> C g y
mapC f ( MkC x ) =
  MkC ( f x )


-- | Map from one column to another, where columns types are subject to a
-- constraint.
mapCC
  :: forall c f g x y null. c x
  => ( c x => MkColumn f ( null x ) -> MkColumn g ( null x ) )
  -> C f ( null x )
  -> C g ( null x )
mapCC f ( MkC x ) =
  MkC ( f x )


-- | Effectfully map from one column to another.
traverseC
  :: Applicative m
  => ( MkColumn f x -> m ( MkColumn g y ) ) -> C f x -> m ( C g y )
traverseC f ( MkC x ) =
  MkC <$> f x


-- | Effectfully map from one column to another, where column types are subject
-- to a constraint.
traverseCC
  :: forall c x y f g m null
   . ( Applicative m, c x, c y )
  => ( ( c x, c y ) => MkColumn f ( null x ) -> m ( MkColumn g ( null y ) ) )
  -> C f ( null x )
  -> m ( C g ( null y ) )
traverseCC f ( MkC x ) =
  MkC <$> f x


-- | If you know two columns are actually equal, @castC@ lets you cast between
-- them.
castC :: MkColumn f a ~ MkColumn g b => C f a -> C g b
castC ( MkC x ) =
  MkC x


-- | If a column contains an effectful operation, sequence that operation into a
--  new column.
sequenceC
  :: ( MkColumn f a ~ m ( MkColumn g y ), Functor m )
  => C f a -> m ( C g y )
sequenceC ( MkC x ) =
  MkC <$> x


-- | Zip two columns together under an effectful context.
zipCWithM
  :: Applicative m
  => ( MkColumn f x -> MkColumn g y -> m ( MkColumn h z ) )
  -> C f x -> C g y -> m ( C h z )
zipCWithM f ( MkC x ) ( MkC y ) =
  MkC <$> f x y


-- | Zip two columns together under an effectful context, where all column types
-- satisfy a common constraint.
zipCWithMC
  :: forall c f g h x y z m null
   . ( Applicative m, c x, c y, c z )
  => ( ( c x, c y, c z ) => MkColumn f ( null x ) -> MkColumn g ( null y ) -> m ( MkColumn h ( null z ) ) )
  -> C f ( null x ) -> C g ( null y ) -> m ( C h ( null z ) )
zipCWithMC f ( MkC x ) ( MkC y ) =
  MkC <$> f x y
