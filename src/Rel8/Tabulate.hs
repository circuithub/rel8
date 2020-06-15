{-# language Arrows #-}
{-# language BlockArguments #-}
{-# language DeriveFunctor #-}
{-# language FlexibleContexts #-}
{-# language TupleSections #-}
{-# language TypeFamilies #-}

module Rel8.Tabulate
  -- ( Key( match )
  -- , Tabulation
  -- , tabulate
  -- , tabulateA
  -- , fromQuery
  -- , precompose
  -- , postcompose
  -- , filter
  -- , indexed
  -- , ifilter
  -- , with
  -- , withBy
  -- , without
  -- , withoutBy
  -- , exists
  -- , lookup
  -- , lookupA
  -- , lookupQ
  -- , lookupAny
  -- , align
  -- , alignWith
  -- , leftAlign
  -- , leftAlignWith
  -- , zip
  -- , zipWith
  -- , similarity
  -- , difference
  -- , aggregateTabulation
  -- )
where

-- base
import Control.Applicative ( liftA2 )
import Control.Arrow
  ( Arrow
  , ArrowApply
  , ArrowChoice
  , Kleisli(..)
  , (&&&)
  , app
  , arr
  , returnA
  , second
  )
import qualified Control.Arrow as A
import Control.Category ( Category, (.), id )
import Control.Monad ( join )
import Data.Bifunctor ( bimap )
import Prelude hiding ( (.), filter, id, lookup, zip, zipWith )

-- opaleye
import Opaleye.Internal.Lateral ( bilaterally, lateral, laterally )

-- product-profunctors
import Data.Profunctor.Product ( ProductProfunctor( (****), purePP ) )

-- profunctors
import Data.Profunctor
  ( Choice
  , Profunctor
  , Strong
  , dimap
  , first'
  , left'
  , lmap
  , right'
  , second'
  )

-- rel8
import Rel8
  ( Context
  , DBEq
  , Expr
  , MaybeTable
  , EqTable
  , Query
  , Table
  , (&&.)
  , (==.)
  , filter
  , lit
  , not_
  , optional
  , where_
  )

-- semigroupoids
import Data.Functor.Apply ( Apply, liftF2 )
import Data.Functor.Bind ( Bind )
import qualified Data.Functor.Bind
import Data.Semigroupoid ( Semigroupoid, o )


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
newtype Tabulation k i a = Tabulation (Query k -> Kleisli Query i (k, a))
  deriving Functor


instance Profunctor (Tabulation k) where
  dimap f g (Tabulation tabulation) =
    Tabulation $ dimap f (fmap g) . tabulation


instance Strong (Tabulation k) where
  second' (Tabulation f) = Tabulation $ fmap twist . second . f
    where
      twist (c, (k, b)) = (k, (c, b))


instance Choice (Tabulation k) where
--   right' (Tabulation f) = Tabulation $ \ks ->
--     lateral (either (\c -> fmap (, Left c) ks) (pure . fmap Right))
--       . A.right (f ks)


instance EqTable k => ProductProfunctor (Tabulation k) where
  purePP = pure
  (****) = (<*>)


instance EqTable k => Semigroupoid (Tabulation k) where
  o = (.)


instance EqTable k => Category (Tabulation k) where
  id = Tabulation $ \ks -> Kleisli \i -> do
    k <- ks
    return (k, i)
  Tabulation f . Tabulation g = Tabulation $ \ks -> Kleisli \i -> do
    (k, a) <- runKleisli (g ks) i
    (l, b) <- runKleisli (f ks) a
    where_ $ k ==. l
    return (k, b)


instance EqTable k => Arrow (Tabulation k) where
  arr f = fmap f id
  first = first'


instance EqTable k => ArrowChoice (Tabulation k) where
  left = left'


instance EqTable k => ArrowApply (Tabulation k) where
  app = Tabulation $ \ks -> lmap (bimap (\(Tabulation query) -> query ks) id) app


instance EqTable k => Apply (Tabulation k i) where
  liftF2 = liftA2


instance EqTable k => Applicative (Tabulation k i) where
  pure a = Tabulation $ \ks -> Kleisli \_ -> do
    k <- ks
    return (k, a)
  liftA2 = zipWith


instance EqTable k => Bind (Tabulation k i) where
  join = join


instance EqTable k => Monad (Tabulation k i) where
  Tabulation query >>= f = Tabulation \ks -> do
    (k, a) <- query ks
    case f a of
      Tabulation query' -> Kleisli (filter ((k ==.) . fst)) . query' ks


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
tabulate :: (a -> Query k) -> Tabulation k a a
tabulate key = fromQuery \a -> (,a) <$> key a


-- | Analgous to 'Data.Map.Strict.fromList'.
fromQuery :: (i -> Query (k, a)) -> Tabulation k i a
fromQuery = Tabulation . const . Kleisli


indexed :: Tabulation k i a -> Tabulation k i (k, a)
indexed (Tabulation tabulation) = Tabulation $ fmap (liftA2 (,) fst id) . tabulation


ifilter :: (k -> a -> Expr Bool) -> Tabulation k i a -> Tabulation k i a
ifilter f tabulation = snd <$> filter (uncurry f) `postcompose` indexed tabulation


-- | 'with' is like 'similarity', but for 'QueryArr's rather than
-- 'Tabulation's. @'with' barsFromFoo . allFoos@ will get you all @Foo@s that
-- have @Bar@s associated with them.
--
-- Note that, as 'similarity' is to @'zipWith' const@, 'with' is to
-- @liftA2 const id@. To use the example above, @'with' barsFromFoo@ will
-- return at most one @Foo@ for each @Foo@ it's given — this is different from
-- 'liftA2 const id barsFromFoo', which will would return 3 copies of a given
-- @Foo@ if that @Foo@ had 3 @Bar@s associated with it.
-- with :: (a -> Query b) -> a -> Query a
-- with = withBy (\_ _ -> lit True) returnA


-- | @'withBy' f as bs@ means: give me all the @as@ for which there exists
-- @bs@ that would satisfy the predicate @f a b@.
-- withBy ::
--   => (a -> b -> Expr bool) -> (i -> Query a) -> QueryArr i b -> QueryArr i a
-- withBy predicate as bs = proc i -> do
--   a <- as -< i
--   restrictExists (filterA bs) -< (predicate a, i)
--   returnA -< a


-- -- | 'without' is like 'difference', but for 'QueryArr's rather than
-- -- 'Tabulation's. @'without' barsFromFoo . allFoos@ will get you all @Foo@s
-- -- that don't have any @Bar@s associated with them.
-- without :: QueryArr a b -> QueryArr a a
-- without = withoutBy (\_ _ -> lit True) returnA


-- -- | @'withoutBy' f as bs@ means: give me all the @as@ for which there are no
-- -- @bs@ that would satisfy the predicate @f a b@.
-- withoutBy :: Predicate bool
--   => (a -> b -> Expr bool) -> QueryArr i a -> QueryArr i b -> QueryArr i a
-- withoutBy predicate as bs = proc i -> do
--   a <- as -< i
--   restrictNotExists (filterA bs) -< (predicate a, i)
--   returnA -< a


-- | 'exists', given a 'QueryArr', lets you test, for any given input to that
-- 'QueryArr', if would it have returned any rows.
-- exists :: (a -> Query b) -> a -> Query (Expr Bool)
-- exists = fmap (not_ . isTableNull) . optional . with


-- | Map a 'QueryArr' over the input side of a 'Tabulation'. In particular,
-- you can turn a @'Tabulation' k i a@ into a @'Tabulation' k () a@
-- (for use with 'leftAlign' and friends) by 'precompose'ing with
-- 'queryTable', like this:
--
-- @
-- tabulation `'precompose'` 'queryTable'
-- @
precompose :: Tabulation k i a -> (j -> Query i) -> Tabulation k j a
precompose (Tabulation f) g = Tabulation $ (. Kleisli g) . f


infixr 8 `precompose`


-- | Map a 'QueryArr' over the output side of a 'Tabulation'.
postcompose :: (a -> Query b) -> Tabulation k i a -> Tabulation k i b
postcompose f (Tabulation g) = Tabulation $ (second (Kleisli f) .) . g


infixr 8 `postcompose`


-- | Note that because 'Tabulation' is a @MultiMap@, the 'Query' returned by
-- 'lookup' can and often does contain multiple results.
lookup :: EqTable k => k -> Tabulation k i a -> i -> Query a
lookup = lookupAny . pure


-- -- | A variant of 'lookupA' that 'precompose's with 'queryTable'.
-- lookupQ :: (Key k, BaseTable t, table ~ t Expr)
--   => Tabulation k table a -> QueryArr k a
-- lookupQ = lookupA . (`precompose` queryTable)


-- | 'lookupAny' is like 'lookup', but instead of taking a single @k@, it
-- takes a 'Query' of @k@s, and the resulting query will include @a@s that
-- 'match'ed /any/ of the given @k@s. This is often used with 'Rel8.values'.
lookupAny :: EqTable k => Query k -> Tabulation k i a -> i -> Query a
lookupAny ks (Tabulation tabulation) i = do
  (k, a) <- runKleisli (tabulation ks) i
  l <- ks
  where_ $ k ==. l
  return a


-- -- | Analagous to [@align@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:align).
-- --
-- -- If 'zip' makes an @INNER JOIN@, then 'align' makes a @FULL OUTER
-- -- JOIN@. One caveat is that the 'Tabulation's must have their inputs
-- -- pre-applied before they can be 'align'ed, which you can do with
-- -- 'precompose'.
-- align :: (Key k, Table k _k, Table a _a, Table b _b)
--   => Tabulation k i a -> Tabulation k i b -> Tabulation k i (TheseTable a b)
-- align = alignWith id


-- -- | Analagous to [@alignWith@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:alignWith).
-- --
-- -- See 'zipWith' and 'align'.
-- alignWith :: (Key k, Table k _k, Table a _a, Table b _b)
--   => (TheseTable a b -> c)
--   -> Tabulation k i a -> Tabulation k i b -> Tabulation k i c
-- alignWith f (Tabulation left) (Tabulation right) = Tabulation
--   $ fmap (theseTable fst fst (const fst) &&& f . bimap snd snd)
--   . liftA2 (bilaterally (fullJoin (\(k, _) (l, _) -> match k l))) left right


-- | If 'zip' makes an @INNER JOIN@, then 'leftAlign' makes a @LEFT JOIN@.
-- This means it will return at least one row for every row in the left
-- 'Tabulation', even if there is no corresponding row in the right (hence
-- the 'MaybeTable').
--
-- Analagous to [@rpadZip@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:rpadZip).
leftAlign :: EqTable k
  => Tabulation k i a -> Tabulation k i b -> Tabulation k i (a, MaybeTable b)
leftAlign = leftAlignWith (,)


-- | See 'zipWith' and 'leftAlign'.
--
-- Analagous to [@rpadZipWith@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:rpadZipWith).
leftAlignWith :: EqTable k
  => (a -> MaybeTable b -> c)
  -> Tabulation k i a -> Tabulation k i b -> Tabulation k i c
leftAlignWith f (Tabulation left) (Tabulation right) = Tabulation $ \ks -> Kleisli \i -> do
  (k, a) <- runKleisli (left ks) i
  mlb <- optional $ filter ((k ==.) . fst) =<< runKleisli (right ks) i
  return (k, f a (snd <$> mlb))


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
  => Tabulation k i a -> Tabulation k i b -> Tabulation k i (a, b)
zip = zipWith (,)


-- | Analagous to [@zipWith@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:zipWith).
-- See 'zip'.
zipWith :: EqTable k
  => (a -> b -> c) -> Tabulation k i a -> Tabulation k i b -> Tabulation k i c
zipWith f (Tabulation left) (Tabulation right) = Tabulation \ks -> Kleisli \i -> do
  (k, a) <- runKleisli (left ks) i
  (l, b) <- runKleisli (right ks) i
  where_ $ k ==. l
  return (k, f a b)


-- -- | 'similarity' returns all the entries in the left 'Tabulation' that have
-- -- a corresponding entry in the right 'Tabulation'. This corresponds to a
-- -- semijoin in relational algebra.
-- --
-- -- This differs from @'zipWith' const x y@ when the right 'Tabulation' @y@
-- -- contains an entry with multiple rows. For 'similarity', the entries in the
-- -- resulting 'Tabulation' will contain the same number of rows as their
-- -- respective entries in the left 'Tabulation' @x@. With `zipWith const x y`,
-- -- each entry would contain the /product/ of the number of rows of their
-- -- respective entries in @x@ and @y@.
-- --
-- -- See 'with'.
-- similarity :: Key k => Tabulation k i a -> Tabulation k i b -> Tabulation k i a
-- similarity (Tabulation left) (Tabulation right) = Tabulation $
--   withBy (\(k, _) (l, _) -> match k l) <$> left <*> right


-- -- | 'difference' returns all the entries in the left 'Tabulation' that don't
-- -- exist in the right 'Tabulation'. This corresponds to an antijoin in
-- -- relational algebra.
-- --
-- -- See 'without'.
-- difference :: Key k => Tabulation k i a -> Tabulation k i b -> Tabulation k i a
-- difference (Tabulation left) (Tabulation right) = Tabulation $
--   withoutBy (\(k, _) (l, _) -> match k l) <$> left <*> right


-- aggregateTabulation :: (AggregateTable b c, Key k, Table c _c, Table k _k, Key k)
--   => (a -> b) -> Tabulation k i a -> Tabulation k i c
-- aggregateTabulation agg (Tabulation query) =
--   Tabulation (laterally aggregate . fmap (bimap groupKey agg) . query)
