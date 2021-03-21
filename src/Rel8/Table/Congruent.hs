{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}

module Rel8.Table.Congruent ( Congruent, mapTable, zipTablesWithM, traverseTable ) where

-- base
import Control.Applicative ( Applicative( liftA2 ) )
import Data.Functor.Identity ( Identity( runIdentity ) )

-- rel8
import Rel8.Context ( Column( ComposedColumn ), Meta( Meta ), decompose )
import Rel8.HTable ( hfield, htabulateMeta, htraverseMeta )
import Rel8.Table ( Table( Columns, fromColumns, toColumns ) )

-- semigroupoids
import Data.Functor.Apply ( Apply )


-- | We say that two 'Table's are congruent if they have the same set of
-- columns. This is primarily useful for operations like @SELECT FROM@, where
-- we have a @Table@ of @ColumnSchema@s, and need to select them to a
-- corresponding @Table@ of @Expr@s.
class (Columns a ~ Columns b) => Congruent a b


instance (Columns a ~ Columns b) => Congruent a b


mapTable
  :: (Congruent s t, Table f s, Table g t)
  => (forall x d y. x ~ 'Meta d y => Column f x -> Column g x) -> s -> t
mapTable f = fromColumns . runIdentity . htraverseMeta (pure . f) . toColumns


zipTablesWithM
  :: forall x y z f g h m
   . (Congruent x y, Columns y ~ Columns z, Table f x, Table g y, Table h z, Apply m)
  => (forall a d b. a ~ 'Meta d b => Column f a -> Column g a -> m (Column h a)) -> x -> y -> m z
zipTablesWithM f (toColumns -> x) (toColumns -> y) =
  fmap fromColumns $
    htraverseMeta decompose $
      htabulateMeta $
        ComposedColumn . liftA2 f (hfield x) (hfield y)


traverseTable
  :: (Congruent x y, Table f x, Table g y, Apply m)
  => (forall a d b. 'Meta d b ~ a => Column f a -> m (Column g a)) -> x -> m y
traverseTable f = fmap fromColumns . htraverseMeta f . toColumns
