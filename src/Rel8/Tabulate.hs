{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language TupleSections #-}

module Rel8.Tabulate
  ( Tabulation
  , runTabulation
  , tabulate
  , tabulateA
  , fromQuery
  , liftQuery
  , prebind
  , postbind
  , indexed
  , ifilter
  , lookup
  , align
  , alignWith
  , leftAlign
  , leftAlignWith
  , zip
  , zipWith
  , similarity
  , difference
  , aggregateTabulation
  , optionalTabulation
  )
where

-- base
import Control.Applicative ( liftA2 )
import Control.Monad ( join, liftM2 )
import Data.Bifunctor ( bimap, first )
import Data.Foldable ( traverse_ )
import Data.Maybe ( fromMaybe )
import Prelude hiding ( filter, lookup, undefined, zip, zipWith )

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( not_, true )
import Rel8.Query ( Query )
import Rel8.Query.Aggregate ( aggregate )
import Rel8.Query.Exists ( withBy, withoutBy )
import Rel8.Query.Filter ( filter, where_ )
import Rel8.Query.Maybe ( optional )
import Rel8.Query.These ( alignBy )
import Rel8.Table ( Table )
import Rel8.Table.Aggregate ( groupBy )
import Rel8.Table.Eq ( EqTable, (==:) )
import Rel8.Table.Maybe ( MaybeTable )
import Rel8.Table.These ( TheseTable, theseTable )
import Rel8.Table.Undefined ( undefined )

-- semigroupoids
import Data.Functor.Apply ( Apply, liftF2 )
import Data.Functor.Bind ( Bind )
import qualified Data.Functor.Bind
import Data.Semigroup.Traversable.Class ( bitraverse1 )


-- | @'Tabulation' k a@ is denotionally a @MultiMap k a@ — a @Map@ where each
-- key @k@ corresponds to potentially multiple @a@ (i.e., @'Query' a@).  This
-- @MultiMap@ supports 'lookup' and other operations you would expect it to.
--
-- \"Identity\" 'Tabulation's are created using 'tabulate'. 'Tabulation's can
-- be composed with 'Query's with 'prebind' or 'postbind' to form new
-- 'Tabulation's.
newtype Tabulation k a = Tabulation (k -> Query (Maybe k, a))
  deriving stock Functor


instance EqTable k => Apply (Tabulation k) where
  liftF2 = liftA2


instance EqTable k => Applicative (Tabulation k) where
  pure = liftQuery . pure
  liftA2 = liftM2


instance EqTable k => Bind (Tabulation k) where
  join = join


instance EqTable k => Monad (Tabulation k) where
  Tabulation as >>= f = Tabulation $ \i -> do
    (mk, a) <- as i
    case mk of
      Nothing -> case f a of
        Tabulation bs -> bs i
      Just k -> case f a of
        Tabulation bs -> do
          (mk', b) <- bs k
          case mk' of
            Nothing -> pure (mk, b)
            Just k' -> do
              where_ $ k ==: k'
              pure (mk', b)


-- | This "undoes" 'fromQuery'.
--
-- @
-- 'runTabulation' . 'fromQuery' = id
-- 'fromQuery' . 'runTabulation' = id
-- @
--
-- Note however that it produces nonsense when passed a 'Tabulation' that was
-- made with from 'liftQuery' or 'pure'. In such cases the returned 'Query'
-- will always produce zero rows.
runTabulation :: EqTable k => Tabulation k a -> Query (k, a)
runTabulation (Tabulation f) = do
  (mk, a) <- f i
  case mk of
    Nothing -> do
      -- Opaleye tries to be too clever optimising "WHERE FALSE" and generates
      -- invalid SQL, so we use "WHERE NOT TRUE" instead
      where_ $ not_ true
      pure (i, a)
    Just k -> pure (k, a)
  where
    i = undefined


liftQuery :: Query a -> Tabulation k a
liftQuery query = Tabulation $ const $ fmap (Nothing,) query


-- | 'tabulate' creates an \"identity\" @'Tabulation' k a@ that allows @a@ be
-- indexed by one or more of its columns @k@. Some examples:
--
-- [Tabulation by primary key]:
--   @
--   projectsById :: Project 'Expr' -> 'Tabulation' ('Expr' ProjectId) (Project 'Expr')
--   projectsById = 'tabulate' projectId
--   @
--
--   Note: the nature of primary keys means that each key will be mapped to a
--   singleton value in this case.
--
-- [Tabulation by other unique key]:
--   @
--   projectsByName :: Project 'Expr' -> 'Tabulation' ('Expr' Text) (Project 'Expr')
--   projectsByName = 'tabulate' projectName
--   @
--
-- [Tabulation by foreign key (tabulate a child table by parent key)]:
--   @
--   revisionsByProjectId :: Revision 'Expr' -> 'Tabulation' ('Expr' ProjectId) (Revision 'Expr')
--   revisionsByProjectId = 'tabulate' revisionProjectId
--   @
tabulate :: (a -> k) -> a -> Tabulation k a
tabulate key a = fromQuery $ pure (key a, a)


-- | Like 'tabulate' but takes a monadic 'Query' function instead of a pure
-- one.  This means you can filter rows while calculating the key, which is
-- useful in conjunction with 'Rel8.Extra.catNulls'.
tabulateA :: (a -> Query k) -> a -> Tabulation k a
tabulateA key a = fromQuery $ (,a) <$> key a


-- | Analgous to 'Data.Map.Strict.fromList'.
fromQuery :: Query (k, a) -> Tabulation k a
fromQuery = Tabulation . const . fmap (first Just)


indexed :: Tabulation k a -> Tabulation k (k, a)
indexed (Tabulation query) = Tabulation $ \i ->
  (\(mk, a) -> (mk, (fromMaybe i mk, a))) <$> query i


ifilter :: (k -> a -> Expr Bool) -> Tabulation k a -> Tabulation k a
ifilter f tabulation = snd <$> do
  filter (uncurry f) `postbind` indexed tabulation


-- | Map a 'Query' over the input side of a 'Tabulation'. In particular, you
-- can turn a @'Tabulation' k a@ into a @'Tabulation' k a@ (for use with
-- 'leftAlign' and friends) by 'prebind'ing with 'queryTable', like this:
--
-- @
-- tabulation `'prebind'` 'queryTable'
-- @
prebind :: (a -> Tabulation k b) -> Query a -> Tabulation k b
prebind f as = Tabulation $ \k -> do
  a <- as
  case f a of
    Tabulation query -> query k
infixr 1 `prebind`


-- | Map a 'Query' over the output side of a 'Tabulation'.
postbind :: (a -> Query b) -> Tabulation k a -> Tabulation k b
postbind f (Tabulation as) = Tabulation $ \i -> do
  (k, a) <- as i
  case f a of
    bs -> do
      b <- bs
      pure (k, b)
infixr 1 `postbind`


-- | Note that because 'Tabulation' is a @MultiMap@, the 'Query' returned by
-- 'lookup' can and often does contain multiple results.
lookup :: EqTable k => k -> Tabulation k a -> Query a
lookup key (Tabulation query) = do
  (mk, a) <- query key
  traverse_ (where_ . (key ==:)) mk
  pure a


-- | Analagous to
-- [@align@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:align).
--
-- If 'zip' makes an @INNER JOIN@, then 'align' makes a @FULL OUTER JOIN@.
align :: (EqTable k, Table Expr a, Table Expr b)
  => Tabulation k a -> Tabulation k b -> Tabulation k (TheseTable a b)
align = alignWith id


-- | Analagous to
-- [@alignWith@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:alignWith).
--
-- See 'zipWith' and 'align'.
alignWith :: (EqTable k, Table Expr a, Table Expr b)
  => (TheseTable a b -> c)
  -> Tabulation k a -> Tabulation k b -> Tabulation k c
alignWith f kas kbs = do
  as <- toQuery kas
  bs <- toQuery kbs
  fromQuery $ do
    tkab <- alignBy (\(l, _) (m, _) -> l ==: m) as bs
    let
      k' = theseTable fst fst (const fst) tkab
      tab = bimap snd snd tkab
    pure (k', f tab)


-- | If 'zip' makes an @INNER JOIN@, then 'leftAlign' makes a @LEFT JOIN@.
-- This means it will return at least one row for every row in the left
-- 'Tabulation', even if there is no corresponding row in the right (hence the
-- 'MaybeTable').
--
-- Analagous to
-- [@rpadZip@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:rpadZip).
leftAlign :: EqTable k
  => Tabulation k a -> Tabulation k b -> Tabulation k (a, MaybeTable b)
leftAlign = leftAlignWith (,)


-- | See 'zipWith' and 'leftAlign'.
--
-- Analagous to
-- [@rpadZipWith@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:rpadZipWith).
leftAlignWith :: EqTable k
  => (a -> MaybeTable b -> c)
  -> Tabulation k a -> Tabulation k b -> Tabulation k c
leftAlignWith f left right = liftA2 f left (optionalTabulation right)


-- | Analagous to
-- [@zip@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:zip).
--
-- There are multiple correct ways of understanding what this does.
--
-- You can think of it as @'Data.Map.Strict.intersectionWith'
-- ('Control.Applicative.liftA2' (,))@.  That is, @intersect@ the two
-- `Tabulation`s by 'match'ing their keys together, and combine their values
-- (remembering that 'Tabulation' is a 'MultiMap' so that the values are keys)
-- by getting their cartesian product.
--
-- You can think of it as performing a cross product of the underlying 'Query's
-- of the given 'Tabulation's and filtering the results for 'match'ing keys.
--
-- You can think of it as a natural join in SQL terms.
--
-- The size of the resulting 'Tabulation' will be \(\sum_{k} min(n_k, m_k) \)
-- in terms of the number of keys, but \(\sum_{k} n_k \times m_k\) in terms of
-- the number of values.
zip :: EqTable k
  => Tabulation k a -> Tabulation k b -> Tabulation k (a, b)
zip = zipWith (,)


-- | Analagous to
-- [@zipWith@](https://hackage.haskell.org/package/semialign/docs/Data-Semialign.html#v:zipWith).
--
-- See 'zip'.
zipWith :: EqTable k
  => (a -> b -> c) -> Tabulation k a -> Tabulation k b -> Tabulation k c
zipWith = liftA2


-- | 'similarity' returns all the entries in the left 'Tabulation' that have a
-- corresponding entry in the right 'Tabulation'. This corresponds to a
-- semijoin in relational algebra.
--
-- This differs from @'zipWith' const x y@ when the right 'Tabulation' @y@
-- contains an entry with multiple rows. For 'similarity', the entries in the
-- resulting 'Tabulation' will contain the same number of rows as their
-- respective entries in the left 'Tabulation' @x@. With `zipWith const x y`,
-- each entry would contain the /product/ of the number of rows of their
-- respective entries in @x@ and @y@.
--
-- See 'with'.
similarity :: EqTable k => Tabulation k a -> Tabulation k b -> Tabulation k a
similarity kas kbs = do
  as <- toQuery kas
  bs <- toQuery kbs
  fromQuery $ as >>= withBy (\(k, _) (l, _) -> k ==: l) bs


-- | 'difference' returns all the entries in the left 'Tabulation' that don't
-- exist in the right 'Tabulation'. This corresponds to an antijoin in
-- relational algebra.
--
-- See 'without'.
difference :: EqTable k => Tabulation k a -> Tabulation k b -> Tabulation k a
difference kas kbs = do
  as <- toQuery kas
  bs <- toQuery kbs
  fromQuery $ as >>= withoutBy (\(k, _) (l, _) -> k ==: l) bs


aggregateTabulation :: EqTable k
  => Tabulation k (Aggregate a) -> Tabulation k a
aggregateTabulation kas = do
  as <- toQuery kas
  fromQuery $ aggregate $ bitraverse1 groupBy id <$> as


optionalTabulation :: EqTable k
  => Tabulation k a -> Tabulation k (MaybeTable a)
optionalTabulation as = Tabulation $ \k -> do
  ma <- optional $ lookup k as
  pure (Just k, ma)


toQuery :: Tabulation k a -> Tabulation k (Query (k, a))
toQuery (Tabulation as) = Tabulation $ \k ->
  pure (Nothing, first (fromMaybe k) <$> as k)
