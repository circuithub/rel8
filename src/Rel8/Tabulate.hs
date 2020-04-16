{-# language Arrows #-}
{-# language DeriveFunctor #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language TupleSections #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Tabulate where

import Control.Applicative ( liftA2 )
import Control.Arrow ( (&&&), first, returnA, second )
import qualified Control.Arrow as A
import Control.Category ( (<<<) )
import Data.Bifunctor ( bimap )
import Data.Bifunctor.Biff ( Biff(..) )
import Data.Functor.Apply ( Apply, liftF2 )
import Data.Functor.Identity ( Identity(..) )
import Data.Profunctor ( Choice, Profunctor, Strong, dimap, lmap, right', second' )
import Prelude hiding ( filter, lookup, zip, zipWith )
import Rel8.Query
import Rel8.Row
import Rel8.Table


-- | To understand what a @'Tabulation' k i a@ is, consider first a
-- @'Tabulation' k () a. This is denotionally a @MultiMap k a@ — a @Map@ where
-- each key @k@ corresponds to potentially multiple @a@ (i.e., @'Query' a@).
-- This @MultiMap@ supports 'lookup' and other operations you would expect it
-- to.
--
-- A @'Tabulation' k i a@ then is basically an arrowized @MultiMap@;
-- denotationally this is @i -> MultiMap k a@. If you 'lookup' a @k@ in a
-- @'Tabulation' k i a@, you get a @'QueryArr' i a@ instead of a @'Query' a@.
--
-- \"Identity\" 'Tabulation's are created using 'tabulate'. 'Tabulation's can
-- be 'precompose'd or 'postcompose'd with 'QueryArr's to form new
-- 'Tabulation's.
newtype Tabulation k i a = Tabulation (Query i (k, a))
  deriving (Functor)
  deriving (Profunctor) via Biff Query Identity ((,) k)


instance Strong (Tabulation k) where
  second' (Tabulation f) = Tabulation $ twist <$> second f
    where
      twist (c, (k, b)) = (k, (c, b))


instance Monoid k => Choice (Tabulation k) where
  right' (Tabulation f) = Tabulation $ fmap sequence $ A.right f


instance (EqTable k', k ~ Row k') => Apply (Tabulation k i) where
  liftF2 = zipWith


-- | 'tabulate' creates an \"identity\" @'Tabulation' k a a@ that allows @a@
-- be indexed by one or more of its columns @k@. Some examples:
--
-- [Tabulation by primary key]:
--   @
--   projectsById :: 'Tabulation' ('Expr' ProjectId) (Project 'Expr') (Project 'Expr')
--   projectsById = 'tabulate' projectId
--   @
--
--   Note: the nature of primary keys means that each key will be mapped to a
--   singleton value in this case.
--
-- [Tabulation by other unique key]:
--   @
--   projectsByUrn :: 'Tabulation' ('Expr' Urn) (Project 'Expr') (Project 'Expr')
--   projectsByUrn = 'tabulate' projectUrn
--   @
--
-- [Tabulation by foreign key (tabulate a child table by parent key)]:
--   @
--   revisionsByProjectId :: 'Tabulation' ('Expr' ProjectId) (Revision 'Expr') (Revision 'Expr')
--   revisionsByProjectId = 'tabulate' revisionProjectId
--   @
tabulate :: (a -> k) -> Tabulation k a a
tabulate key = Tabulation $ liftA2 (,) key id <$> returnA


-- | 'untabulate' turns a 'Tabulation' back into a 'QueryArr'
untabulate :: Tabulation _k i a -> Query i a
untabulate (Tabulation tabulation) = snd <$> tabulation


keys :: Tabulation k i _a -> Query i k
keys (Tabulation tabulation) = fst <$> tabulation


-- | 'retabulate' re-indexes a 'Tabulation' by a new key @k@ which is given by
-- the first half of the tuple of the output of the prior tabulation.
retabulate :: Tabulation _k i (k, a) -> Tabulation k i a
retabulate = Tabulation . untabulate


-- -- | The main use of 'cotabulate' is to create a 'Tabulation' in the opposite
-- -- direction to what you get with 'tabulate someForeignKey'. Taking our
-- -- 'revisionsByProjectId' example above, we use 'cotabulate' when we want to
-- -- get 'projectsByRevisionId'.
-- --
-- -- [Reverse-tabulating by foreign key (tabulate a parent table by child key)]:
-- --   @
-- --   projectsByRevisionId :: 'Tabulation' ('Expr' RevisionId) (Project 'Expr') (Project 'Expr')
-- --   projectsByRevisionId = 'cotabulate' revisionId revisionsByProjectId projectsById
-- --     where
-- --       revisionsByProjectId = 'tabulate' revisionProjectId
-- --       projectsById = 'tabulate' projectId
-- --   @
-- --
-- -- Unfortunately, 'cotabulate' can't just take @revisionsByProjectId@ and
-- -- reverse it. It needs to know how to get the primary key of the child table
-- -- (@revisionId@), how to tabulate the child table (@Revision@) by the foreign
-- -- key to the parent table (@revisionsByProjectId@) and how to tabulate the
-- -- parent table (@Project@) by this same key (@projectsById@). It returns a
-- -- 'Tabulation' of the parent table by the primary key of the child table
-- -- (@RevisionId@).
-- --
-- -- 'Tabulation's created with 'cotabulate' can themselves be composed, e.g.:
-- --
-- -- @
-- -- revisionsByOrderId :: 'Tabulation' ('Expr' OrderId) (Revision 'Expr') (Revision 'Expr')
-- -- revisionsByOrderId = 'cotabulate' orderId ordersByQuoteId revisionsByQuoteId
-- --  where
-- --    ordersByQuoteId = 'tabulate' orderProjectQuoteId
-- --    revisionsByQuoteId = 'cotabulate' quoteId quotesByRevisionId revisionsById
-- --      where
-- --        quotesByRevisionId = 'tabulate' quoteProjectRevisionId
-- --        revisionsById = 'tabulate' revisionId
-- -- @
-- --
-- -- It isn't also isn't necessary to stick strictly to primary keys when
-- -- creating 'Tabulation's with 'cotabulate'. Here's another example:
-- --
-- -- @
-- -- revisionsByProjectUrn :: 'Tabulation' ('Expr' Urn) (Revision 'Expr') (Revision 'Expr')
-- -- revisionsByProjectUrn = 'cotabulate' projectUrn projectsById revisionsByProjectId
-- --  where
-- --    projectsById = 'tabulate' projectId
-- --    revisionsByProjectId = 'tabulate' revisionProjectId
-- -- @
-- cotabulate :: (Key k, BaseTable t, table ~ t Expr)
--   => (a -> l) -> Tabulation k table a -> Tabulation k i b -> Tabulation l i b
-- cotabulate key f g = retabulate $ zipWith ((,) . key) (precompose f (lmap mempty queryTable)) g


-- -- | `intertabulate` takes two `Tabulation`s with 'match'ing keys @k@, and
-- -- returns a 'QueryArr' whose input @i@ goes into the first 'Tabulation'
-- -- and whose output @b@ comes out of the second 'Tabulation', whereby the
-- -- keys @k@ are used to map inputs @i@ to outputs @b@.
-- intertabulate :: (Key k, BaseTable t, table ~ t Expr)
--   => Tabulation k i a -> Tabulation k table b -> QueryArr i b
-- intertabulate = intertabulateBy match


-- -- | `intertabulateBy` is like 'intertabulate', except you can specify a
-- -- custom equality predicate.
-- intertabulateBy :: (Predicate bool, BaseTable t, table ~ t Expr)
--   => (k -> l -> Expr bool) -> Tabulation k i a -> Tabulation l table b -> QueryArr i b
-- intertabulateBy predicate f g = untabulate $ zipWithBy predicate (const id) f (precompose g (lmap mempty queryTable))


ifilter :: (k -> a -> Row Bool) -> Tabulation k i a -> Tabulation k i a
ifilter f (Tabulation g) = Tabulation $ proc i -> do
  (k, a) <- g -< i
  where_ -< f k a
  returnA -< (k, a)


-- | Map a 'QueryArr' over the input side of a 'Tabulation'. In particular,
-- you can turn a @'Tabulation' k i a@ into a @'Tabulation' k () a@
-- (for use with 'leftAlign' and friends) by 'precompose'ing with
-- 'queryTable', like this:
--
-- @
-- tabulation `'precompose'` 'queryTable'
-- @
precompose :: Tabulation k i a -> Query j i -> Tabulation k j a
precompose (Tabulation f) g = Tabulation $ f <<< g


infixr 8 `precompose`


-- | Map a 'QueryArr' over the output side of a 'Tabulation'.
postcompose :: Query a b -> Tabulation k i a -> Tabulation k i b
postcompose f (Tabulation g) = Tabulation $ second f <<< g


infixr 8 `postcompose`


-- | Map a 'QueryArr' over the keys of a 'Tabulation'.
mapKeys :: Query k l -> Tabulation k i a -> Tabulation l i a
mapKeys f (Tabulation g) = Tabulation $ first f <<< g


-- | Note that because 'Tabulation' is a @MultiMap@, the 'Query' returned by
-- 'lookup' can and often does contain multiple results.
lookup :: EqTable k => Row k -> Tabulation (Row k) i a -> Query i a
lookup = lookupBy . (==.)


-- | Convert a ('precompose'd) 'Tabulation' to a 'QueryArr'. Useful for
-- working inside @proc@ notation when you have a @k@ and just want an @a@.
lookupA :: EqTable k => Tabulation (Row k) () a -> Query (Row k) a
lookupA = lmap (==.) . lookupByA


-- | Like 'lookup' but can take an arbitrary predicate. This can be efficient
-- if @k@ is an indexed column(s) and your predicate is something like
-- @('Rel8.<.' key)@ which uses 'DBOrd'.
--
-- See also 'lookup'.
lookupBy :: (k -> Row Bool) -> Tabulation k i a -> Query i a
lookupBy f (Tabulation tabulation) = proc i -> do
  (k, a) <- tabulation -< i
  where_ -< f k
  returnA -< a


-- | Like 'lookupA' but can take an arbitrary predicate. See also 'lookupBy'.
lookupByA :: Tabulation k () a -> Query (k -> Row Bool) a
lookupByA (Tabulation tabulation) = proc f -> do
  (k, a) <- tabulation -< ()
  where_ -< f k
  returnA -< a


-- | 'lookupAny' is like 'lookup', but instead of taking a single @k@, it
-- takes a 'Query' of @k@s, and the resulting query will include @a@s that
-- 'match'ed /any/ of the given @k@s. This is often used with 'Rel8.values'.
lookupAny :: EqTable k => Query () (Row k) -> Tabulation (Row k) i a -> Query i a
lookupAny = lookupAnyBy (==.)


-- | See 'lookupAny' and 'lookupBy'.
lookupAnyBy :: (k -> l -> Row Bool) -> Query () l -> Tabulation k i a -> Query i a
lookupAnyBy f ks (Tabulation tabulation) = proc i -> do
  (k, a) <- tabulation -< i
  l <- ks -< ()
  where_ -< f k l
  returnA -< a


-- -- | Analagous to [@align@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:align).
-- --
-- -- If 'zip' makes an @INNER JOIN@, then 'align' makes a @FULL OUTER
-- -- JOIN@. One caveat is that the 'Tabulation's must have their inputs
-- -- pre-applied before they can be 'align'ed, which you can do with
-- -- 'precompose'.
-- align :: (Key k, Table k _k, Table a _a, Table b _b)
--   => Tabulation k () a -> Tabulation k () b -> Tabulation k i (TheseTable a b)
-- align = alignWith id


-- -- | Analagous to [@alignWith@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:alignWith).
-- --
-- -- See 'zipWith' and 'align'.
-- alignWith :: (Key k, Table k _k, Table a _a, Table b _b)
--   => (TheseTable a b -> c)
--   -> Tabulation k () a -> Tabulation k () b -> Tabulation k i c
-- alignWith = alignWithBy match


-- -- | See 'zipWithBy' and 'align'
-- alignWithBy :: (Predicate bool, Table k _k, Table a _a, Table b _b)
--   => (k -> k -> Expr bool)
--   -> (TheseTable a b -> c)
--   -> Tabulation k () a -> Tabulation k () b -> Tabulation k i c
-- alignWithBy predicate value (Tabulation left) (Tabulation right) = Tabulation $
--   dimap mempty (theseTable fst fst (const fst) &&& value . bimap snd snd) $
--     fullJoin (\(k, _) (l, _) -> predicate k l) left right


-- | If 'zip' makes an @INNER JOIN@, then 'leftAlign' makes a @LEFT JOIN@.
-- This means it will return at least one row for every row in the left
-- 'Tabulation', even if there is no corresponding row in the right (hence
-- the 'MaybeTable').
--
-- Analagous to [@rpadZip@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:rpadZip).
leftAlign :: (EqTable k, Table b)
  => Tabulation (Row k) i a
  -> Tabulation (Row k) i (Row b)
  -> Tabulation (Row k) i (a, Row (Maybe b))
leftAlign = leftAlignWith (,)


-- | See 'zipWith' and 'leftAlign'.
--
-- Analagous to [@rpadZipWith@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:rpadZipWith).
leftAlignWith :: (EqTable k, Table b)
  => (a -> Row (Maybe b) -> c)
  -> Tabulation (Row k) i a
  -> Tabulation (Row k) i (Row b)
  -> Tabulation (Row k) i c
leftAlignWith = leftAlignWithBy (==.)


-- | See 'zipWithBy' and 'leftAlign'
leftAlignWithBy
  :: (Table b, Table l)
  => (k -> Row l -> Row Bool)
  -> (a -> Row (Maybe b) -> c)
  -> Tabulation k i a
  -> Tabulation (Row l) i (Row b)
  -> Tabulation k i c
leftAlignWithBy predicate value (Tabulation left) (Tabulation right) = Tabulation $ proc i -> do
  (k, a) <- left -< i
  mlb <- optional (filterA (toRow <$> right)) -< (predicate k . fst . fromRow, i)
  returnA -< (k, value a (underRowProduct (fmap (snd . fromRow)) mlb))


-- | Analagous to [@zip@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:zip).
--
-- There are multiple correct ways of understanding what this does.
--
-- You can think of it as
-- @'Data.Map.Strict.intersectionWith' ('Control.Applicative.liftA2' (,))@.
-- That is, @intersect@ the two `Tabulation`s by 'match'ing their keys
-- together, and combine their values (remembering that 'Tabulation' is a
-- 'MultiMap' so that the values are keys) by getting their cartesian product.
--
-- You can think of it as performing a cross product of the underlying
-- 'Query's of the given 'Tabulation's and filtering the results for
-- 'match'ing keys.
--
-- You can think of it as a natural join in SQL terms.
--
-- The size of the resulting 'Tabulation' will be \(\sum_{k} min(n_k, m_k) \)
-- in terms of the number of keys, but \(\sum_{k} n_k \times m_k\) in terms
-- of the number of values.
zip :: EqTable k
  => Tabulation (Row k) i a
  -> Tabulation (Row k) i b
  -> Tabulation (Row k) i (a, b)
zip = zipWith (,)


-- | Analagous to [@zipWith@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:zipWith).
-- See 'zip'.
zipWith :: EqTable k
  => (a -> b -> c)
  -> Tabulation (Row k) i a
  -> Tabulation (Row k) i b
  -> Tabulation (Row k) i c
zipWith = zipWithBy (==.)


-- | Like 'zipWith' but you can override the equality predicate with something
-- other than 'match'.
zipWithBy :: ()
  => (k -> l -> Row Bool)
  -> (a -> b -> c)
  -> Tabulation k i a -> Tabulation l i b -> Tabulation k i c
zipWithBy predicate value (Tabulation left) (Tabulation right) = Tabulation $ proc i -> do
  (k, a) <- left -< i
  (l, b) <- right -< i
  where_ -< predicate k l
  returnA -< (k, value a b)


-- aggregateTabulation :: (AggregateTable b c, Key k, Table c _c, Table k _k, Key k)
--   => (a -> b) -> Tabulation k () a -> Tabulation k i c
-- aggregateTabulation f ( Tabulation q ) =
--   Tabulation ( lmap mempty ( aggregate ( bimap groupKey f <$> q ) ) )
