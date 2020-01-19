{-# language BlockArguments #-}
{-# language DeriveFunctor #-}
{-# language TypeFamilies #-}

{-| This module exposes an API that you may find useful when working with
multiple related tables, or to build composable queries.

-}
module Rel8.Tabulate
  ( Tabulated( Tabulated )
  , singleton
  , lookup
  , lookupBy
  , zip
  , zipWith
  , izipWith
  , Tabulation( Tabulation )
  , run
  , tabulate
  , cotabulate
  ) where

-- base
import Data.Bifunctor ( Bifunctor, bimap )
import Prelude hiding ( lookup, zip, zipWith )

-- profunctors
import Data.Profunctor ( Profunctor, dimap )

-- rel8
import Rel8 ( EqTable, Expr, ExprIn, MonadQuery, (==.), where_ )

-- | @Tabulated m k a@ can be thought of as a @MultiMap@ from @k@ to @a@.
newtype Tabulated m k a =
  Tabulated ( m ( k, a ) )
  deriving ( Functor )


instance Functor m => Bifunctor ( Tabulated m ) where
  bimap f g ( Tabulated a ) =
    Tabulated ( fmap ( bimap f g ) a )


-- | Construct a @Tabulated@ value that contains a single entry.
singleton :: Applicative m => k -> a -> Tabulated m k a
singleton k a =
  Tabulated ( pure ( k, a ) )


-- | Note that 'Tabulated' is a @MultiMap@, so the 'MonadQuery' returned by
--   'lookup' can and often does contain multiple results.
lookup
  :: ( ExprIn k ~ Expr m, EqTable k, MonadQuery m )
  => k -> Tabulated m k a -> m a
lookup =
  lookupBy . (==.)


-- | Like 'lookup' but can take an arbitrary predicate. This can be efficient
-- if @k@ is an indexed column(s) and your predicate is something like
-- @('Rel8.<.' key)@ which uses 'Rel8.DBOrd'.
--
-- See also 'lookup'.
lookupBy
  :: MonadQuery m
  => ( k -> Expr m Bool ) -> Tabulated m k a -> m a
lookupBy f ( Tabulated query ) = do
  ( k, a ) <-
    query

  a <$ where_ ( f k )


-- | Can also be thought of as @intersection@.
zip
  :: ( EqTable k, ExprIn k ~ Expr m, MonadQuery m )
  => Tabulated m k a -> Tabulated m k b -> Tabulated m k ( a, b )
zip =
  zipWith (,)


-- | Can also be thought of @intersectionWith@.
zipWith
  :: ( EqTable k, ExprIn k ~ Expr m, MonadQuery m )
  => ( a -> b -> c )
  -> Tabulated m k a
  -> Tabulated m k b
  -> Tabulated m k c
zipWith = izipWith . const


-- | Combine two @Tabulated@ queries by equality on their key, but combine their
-- values with access to the matching key.
izipWith
  :: MonadQuery m
  => ( EqTable k, ExprIn k ~ Expr m )
  => ( k -> a -> b -> c )
  -> Tabulated m k a
  -> Tabulated m k b
  -> Tabulated m k c
izipWith =
  izipWithBy (==.) const . const


izipWithBy
  :: MonadQuery m
  => ( x -> y -> Expr m Bool )
  -> ( x -> y -> z )
  -> ( x -> y -> a -> b -> c )
  -> Tabulated m x a
  -> Tabulated m y b
  -> Tabulated m z c
izipWithBy f key value ( Tabulated left ) ( Tabulated right ) = Tabulated do
  ( ( k, a ), ( l, b ) ) <-
    (,) <$> left <*> right

  where_ ( f k l )

  return ( key k l, value k l a b )


-- | A @'Tabulation' k i a@ is the ability to transform 'MonadQuery's of
-- @i@ into 'Tabulated's of @a@ indexed by @k@.
--
-- 'Tabulation's are built using 'tabulate' and composed using 'cotabulate'.
--
-- You need 'run' to actually use a 'Tabulation'.
newtype Tabulation m k i a =
  Tabulation
    { -- | 'run' turns a @'Tabulation' k i a@ into its underlying
      -- @'MonadQuery' m i -> 'Tabulated' k a@ function. You generally feed that
      -- a query returning all rows in a table, and then use 'lookup' and
      -- friends on the resulting 'Tabulated' to get 'Rel8.select'able
      -- 'MonadQuery's.
      run :: m i -> Tabulated m k a
    }
  deriving
    ( Functor )


instance Functor m => Profunctor ( Tabulation m k ) where
  dimap f g ( Tabulation t ) =
    Tabulation ( dimap ( fmap f ) ( fmap g ) t )


-- | 'tabulate' creates a @'Tabulation' k i a@ that can index a 'MonadQuery' of
-- @a@ by one or more of the columns @k@ of @a@. Some examples:
--
-- [Tabulation by primary key]:
--   @
--   projectsById :: 'Tabulation' ('Expr' ProjectId) (Project 'Expr') (Project 'Expr')
--   projectsById = 'tabulate' projectId
--   @
--
-- [Tabulation by other unique key]:
--   @
--   projectsByUrn :: 'Tabulation' ('Expr' Urn) (Project 'Expr') (Project 'Expr')
--   projectsByUrn = 'tabulate' projectUrn
--   @
--
-- [Tabulation by foreign key (the ability to tabulate a child table by parent key)]:
--   @
--   revisionsByProjectId :: 'Tabulation' ('Expr' ProjectId) (Revision 'Expr') (Revision 'Expr')
--   revisionsByProjectId = 'tabulate' revisionProjectId
--   @
tabulate :: Functor m => ( a -> k ) -> Tabulation m k a a
tabulate key =
  Tabulation ( Tabulated . fmap go )
  where
    go row = ( key row, row )


-- | The main use of 'cotabulate' is to create a 'Tabulation' in the opposite
-- direction to what you get with 'tabulate someForeignKey'. Taking our
-- @revisionsByProjectId@ example above, we use 'cotabulate' when we want to
-- get @projectsByRevisionId@.
--
-- [Reverse-tabulating by foreign key (the ability to tabulate a parent table by child key)]:
--   @
--   projectsByRevisionId :: 'Tabulation' ('Expr' RevisionId) (Project 'Expr') (Project 'Expr')
--   projectsByRevisionId = 'cotabulate' revisionId revisionsByProjectId projectsById
--     where
--       revisionsByProjectId = 'tabulate' revisionProjectId
--       projectsById = 'tabulate' projectId
--   @
--
-- Unfortunately, 'cotabulate' can't just take @revisionsByProjectId@ and
-- reverse it. It needs to know how to get the primary key of the child table
-- (@revisionId@), how to tabulate the child table (@Revision@) by the foreign
-- key to the parent table (@revisionsByProjectId@) and how to tabulate the
-- parent table (@Project@) by this same key (@projectsById@). It returns
-- the ability to tabulate the parent table by the primary key of the child
-- table (@RevisionId@).
--
-- 'Tabulation's created with 'cotabulate' can themselves be composed, e.g.:
--
-- @
-- revisionsByOrderId :: 'Tabulation' ('Expr' OrderId) (Revision 'Expr') (Revision 'Expr')
-- revisionsByOrderId = 'cotabulate' orderId ordersByQuoteId revisionsByQuoteId
--  where
--    ordersByQuoteId = 'tabulate' orderProjectQuoteId
--    revisionsByQuoteId = 'cotabulate' quoteId quotesByRevisionId revisionsById
--      where
--        quotesByRevisionId = 'tabulate' quoteProjectRevisionId
--        revisionsById = 'tabulate' revisionId
-- @
--
-- It isn't also isn't necessary to stick strictly to primary keys when
-- creating 'Tabulation's with 'cotabulate'. Here's another example:
--
-- @
-- revisionsByProjectUrn :: 'Tabulation' ('Expr' Urn) (Revision 'Expr') (Revision 'Expr')
-- revisionsByProjectUrn = 'cotabulate' projectUrn projectsById revisionsByProjectId
--  where
--    projectsById = 'tabulate' projectId
--    revisionsByProjectId = 'tabulate' revisionProjectId
-- @
cotabulate
  :: ( ExprIn k ~ Expr m, EqTable k, MonadQuery m )
  => ( a -> l )
  -> Tabulated m k a
  -> Tabulation m k j b
  -> Tabulation m l j b
cotabulate key x (Tabulation g) =
  Tabulation $ retabulate . zip x . g
  where
    retabulate (Tabulated query) = Tabulated $ fmap go query
      where
        go (_, (left, right)) = (key left, right)
