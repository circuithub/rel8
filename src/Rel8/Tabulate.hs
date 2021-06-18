{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TupleSections #-}
{-# language UndecidableInstances #-}

-- | "Rel8.Tabulate" provides an alternative API ('Tabulation') for writing
-- queries that complements the main "Rel8" API ('Query').

module Rel8.Tabulate
  (
    Tabulation

    -- * Interfacing with 'Query's
  , fromQuery
  , toQuery
  , liftQuery
  , through
  , lookup

    -- * Aggregation and Ordering
  , aggregateTabulation
  , distinctTabulation
  , orderTabulation

    -- ** Magic 'Tabulation's
    -- $magic
  , countTabulation
  , optionalTabulation
  , manyTabulation
  , someTabulation
  , existsTabulation
  , presentTabulation
  , absentTabulation

    -- * Natural joins
  , align
  , alignWith
  , leftAlign
  , leftAlignWith
  , rightAlign
  , rightAlignWith
  , zip
  , zipWith
  , similarity
  , difference
  )
where

-- base
import Control.Applicative ( (<|>), empty, liftA2 )
import Control.Monad ( liftM2 )
import Data.Bifunctor ( Bifunctor, bimap, first, second )
import Data.Foldable ( traverse_ )
import Data.Function ( on )
import Data.Functor.Contravariant ( Contravariant, (>$<), contramap )
import Data.Int ( Int64 )
import Data.Kind ( Type )
import Data.Maybe ( fromMaybe )
import Prelude hiding ( lookup, zip, zipWith )

-- bifunctors
import Data.Bifunctor.Clown ( Clown( Clown ), runClown )

-- comonad
import Control.Comonad ( extract )

-- opaleye
import qualified Opaleye.Aggregate as Opaleye
import qualified Opaleye.Internal.Order as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Order as Opaleye ( orderBy )

-- profunctors
import Data.Profunctor ( dimap, lmap )

-- product-profunctors
import Data.Profunctor.Product
  ( ProductProfunctor, (***!)
  , SumProfunctor, (+++!)
  )
import qualified Data.Profunctor.Product as PP

-- rel8
import Rel8.Aggregate ( Aggregates )
import Rel8.Expr ( Expr )
import Rel8.Expr.Aggregate ( countStar )
import Rel8.Expr.Bool ( true )
import Rel8.Order ( Order( Order ) )
import Rel8.Query ( Query )
import Rel8.Query.Exists ( exists, present, absent )
import Rel8.Query.Filter ( where_ )
import Rel8.Query.List ( catNonEmptyTable )
import Rel8.Query.Maybe ( optional )
import Rel8.Query.Opaleye ( mapOpaleye )
import Rel8.Query.These ( alignBy )
import Rel8.Table ( Table, fromColumns, toColumns )
import Rel8.Table.Aggregate ( hgroupBy, listAgg, nonEmptyAgg )
import Rel8.Table.Alternative
  ( AltTable, (<|>:)
  , AlternativeTable, emptyTable
  )
import Rel8.Table.Cols ( fromCols, toCols )
import Rel8.Table.Eq ( EqTable, (==:), eqTable )
import Rel8.Table.List ( ListTable( ListTable ) )
import Rel8.Table.Maybe ( MaybeTable( MaybeTable ), maybeTable )
import Rel8.Table.NonEmpty ( NonEmptyTable( NonEmptyTable ) )
import Rel8.Table.Opaleye ( aggregator, unpackspec )
import Rel8.Table.Ord ( OrdTable )
import Rel8.Table.Order ( ascTable )
import Rel8.Table.Projection
  ( Biprojectable, biproject
  , Projectable, project
  , apply
  )
import Rel8.Table.These ( TheseTable( TheseTable ), theseTable )

-- semigroupoids
import Data.Functor.Apply ( Apply, liftF2 )
import Data.Functor.Bind ( Bind, (>>-) )


type Key :: Type -> Type
type Key = Maybe


cat :: Table Expr k => Key k -> Query k
cat = maybe emptyTable pure


key :: (ProductProfunctor p, SumProfunctor p)
  => p a b -> p (Key a) (Key b)
key a = dimap from to (PP.empty +++! a)
  where
    from = maybe (Left ()) Right
    to = either (const Nothing) Just


keyed :: (ProductProfunctor p, SumProfunctor p)
  => p k l -> p a b -> p (Key k, a) (Key l, b)
keyed k a = key k ***! a


type Predicate :: Type -> Type
newtype Predicate a = Predicate (Maybe (a -> Expr Bool))


instance Contravariant Predicate where
  contramap f (Predicate a) = Predicate (lmap f <$> a)


instance Semigroup (Predicate k) where
  Predicate ma <> Predicate mb = Predicate $ ma <|> mb


instance Monoid (Predicate k) where
  mempty = Predicate Nothing


match :: EqTable k => Key k -> Predicate k
match = Predicate . fmap (==:)


ensure :: Predicate k -> Key k -> Query ()
ensure (Predicate mp) = traverse_ (\k -> traverse_ (\p -> where_ (p k)) mp)


-- | A @'Tabulation' k a@ is like a @'Query' a@, except that each row also
-- has a key @k@ in addition to the value @a@. 'Tabulation's can be composed
-- monadically just like 'Query's, but the resulting join is more like a
-- @NATURAL JOIN@ (based on the common key column(s) @k@) as opposed to the
-- @CROSS JOIN@ given by 'Query'.
--
-- Another way to think of @'Tabulation' k a@ is as analogous to @Map k a@ in
-- the same way @'Query' a@ is analogous to @[a]@. However, there's nothing
-- stopping a 'Tabulation' from containing multiple rows with the same key, so
-- technically @Map k (NonEmpty a)@ is more accurate.
type Tabulation :: Type -> Type -> Type
newtype Tabulation k a = Tabulation (Predicate k -> Query (Key k, a))


instance Biprojectable Tabulation where
  biproject f g =
    bimap
      (fromColumns . apply f . toColumns)
      (fromColumns . apply g . toColumns)


instance Bifunctor Tabulation where
  bimap f g (Tabulation a) = Tabulation $ \p ->
    bimap (fmap f) g <$> a (f >$< p)


instance Functor (Tabulation k) where
  fmap = second


instance Projectable (Tabulation k) where
  project f = fmap (fromColumns . apply f . toColumns)


-- | If @'Tabulation' k a@ is @Map k (NonEmpty a)@, then @(<.>)@ is
-- @intersectionWith (liftA2 (<*>))@
instance EqTable k => Apply (Tabulation k) where
  liftF2 = liftA2


-- | @pure = 'liftQuery' . pure@
instance EqTable k => Applicative (Tabulation k) where
  pure = liftQuery . pure
  liftA2 = liftM2


instance EqTable k => Bind (Tabulation k) where
  Tabulation as >>- f = Tabulation $ \p -> do
    (k, a) <- as p
    case f a of
      Tabulation bs -> do
        let p' = match k
        (k', b) <- bs (p' <> p)
        ensure p' k'
        pure (k' <|> k, b)


instance EqTable k => Monad (Tabulation k) where
  (>>=) = (>>-)


-- | If @'Tabulation' k a@ is @Map k (NonEmpty a)@, then @(<|>:)@ is
-- @unionWith (<>)@.
instance EqTable k => AltTable (Tabulation k) where
  as <|>: bs = catNonEmptyTable `through` ((<>) `on` someTabulation) as bs


instance EqTable k => AlternativeTable (Tabulation k) where
  emptyTable = Tabulation $ const $ fmap (empty,) emptyTable


-- | If @'Tabulation' k a@ is @Map k (NonEmpty a)@, then @(<>)@ is
-- @unionWith (liftA2 (<>))@.
instance (EqTable k, Table Expr a, Semigroup a) => Semigroup (Tabulation k a)
 where
  (<>) = alignWith (theseTable id id (<>))


instance (EqTable k, Table Expr a, Semigroup a) => Monoid (Tabulation k a)
 where
  mempty = emptyTable


-- | Any 'Query' of key-value pairs @(k, a)@ can be a @'Tabulation' k a@.
fromQuery :: Query (k, a) -> Tabulation k a
fromQuery = Tabulation . const . fmap (first pure)


-- | Convert a @'Tabulation' k a@ back into a 'Query' of key-value pairs.
--
-- Note that the result of a 'toQuery' is undefined (will always return zero
-- rows) on 'Tabulation's constructed with 'liftQuery' or 'pure'. So while
-- @toQuery . fromQuery@ is always @id@, @fromQuery . toQuery@ is not.
-- 'toQuery' is therefore mostly only intended for debugging.
--
-- A safer, more predictable alternative to 'toQuery' is to use 'lookup' with
-- an explicit set of keys:
--
-- @
-- do
--    k <- keys
--    a <- lookup k tabulation
--    pure (k, a)
-- @
toQuery :: Table Expr k => Tabulation k a -> Query (k, a)
toQuery (Tabulation f) = do
  (mk, a) <- f mempty
  k <- cat mk
  pure (k, a)


-- | A @'Query' a@ can be treated as a @'Tabulation' k a@ where the given @a@
-- values exist at every possible key @k@.
liftQuery :: Query a -> Tabulation k a
liftQuery = Tabulation . const . fmap (empty,)


-- | Run a Kleisli arrow in the the 'Query' monad \"through\" a 'Tabulation'.
-- Useful for 'Rel8.filter'ing a 'Tabulation'.
--
-- @
-- 'Rel8.filter' ((>=. 30) . userAge) `'through'` usersById
-- @
through :: (a -> Query b) -> Tabulation k a -> Tabulation k b
through f (Tabulation as) = Tabulation $ \p -> do
  (k, a) <- as p
  b <- f a
  pure (k, b)
infixr 1 `through`


-- | @'lookup' k t@ returns the value(s) at the key @k@ in the tabulation @t@.
lookup :: EqTable k => k -> Tabulation k a -> Query a
lookup k (Tabulation f) = do
  (k', a) <- f p
  ensure p k'
  pure a
  where
    p = match (pure k)


-- | 'aggregateTabulation' aggregates the values within each key of a
-- 'Tabulation'. There is an implicit @GROUP BY@ on all the key columns.
aggregateTabulation :: forall k aggregates exprs.
  ( EqTable k
  , Aggregates aggregates exprs
  )
  => Tabulation k aggregates -> Tabulation k exprs
aggregateTabulation (Tabulation f) = Tabulation $
  mapOpaleye (Opaleye.aggregate (keyed haggregator aggregator)) .
  fmap (first (fmap (hgroupBy (eqTable @k) . toColumns))) .
  f
  where
    haggregator = dimap fromColumns fromCols aggregator


-- | 'distinctTabulation' ensures a 'Tabulation' has at most one value for
-- each key, i.e., it drops duplicates. In general it keeps only the
-- \"first\" value it encounters for each key, but note that \"first\" is
-- undefined unless you first call 'orderTabulation'.
distinctTabulation :: EqTable k => Tabulation k a -> Tabulation k a
distinctTabulation (Tabulation f) = Tabulation $
  mapOpaleye
    (\q ->
      Opaleye.productQueryArr
        ( Opaleye.distinctOn (key unpackspec) fst
        . Opaleye.runSimpleQueryArr q
        )
    ) .
  f


-- | 'orderTabulation' orders the /values/ of a 'Tabulation' within their
-- respective keys. This specifies a defined order for 'distinctTabulation'.
-- It also defines the order of the lists produced by 'manyTabulation' and
-- 'someTabulation'.
orderTabulation :: OrdTable k => Order a -> Tabulation k a -> Tabulation k a
orderTabulation ordering (Tabulation f) =
  Tabulation $ mapOpaleye (Opaleye.orderBy ordering') . f
  where
    Order ordering' = runClown (keyed (Clown ascTable) (Clown ordering))


-- $magic
--
-- Some of the following combinators produce \"magic\" 'Tabulation's. Let's
-- use 'countTabulation' as an example to demonstrate this concept. Consider
-- the following:
--
-- @
-- countTabulation $ fromQuery $ values
--   [ (lit 'a', lit True)
--   , (lit 'a', lit False)
--   , (lit 'b', lit True)
--   ]
-- @
--
-- You might expect this to be equivalent to the following 'Tabulation':
--
-- @
-- fromQuery $ values
--   [ (lit 'a', 2)
--   , (lit 'b', 1)
--   ]
-- @
--
-- However, it isn't quite. While the resulting 'Tabulation' does effectively
-- contain the above entries, it also behaves as though it contained the value
-- @0@ at every other possible key.
--
-- This means you can do:
--
-- @
-- do
--   user <- usersById
--   orderCount <- countTabulation ordersByUserId
-- @
--
-- To see how many orders a user has (getting @0@ if they have no orders).


-- | 'countTabulation' returns a count of how many entries are in the given
-- 'Tabulation' at each key.
--
-- The resulting 'Tabulation' is \"magic\" in that the value @0@ exists at
-- every possible key that wasn't in the given 'Tabulation'.
countTabulation :: EqTable k => Tabulation k a -> Tabulation k (Expr Int64)
countTabulation =
  fmap (maybeTable 0 id) .
  optionalTabulation .
  aggregateTabulation .
  fmap (const countStar)


-- | 'optionalTabulation' produces a \"magic\" 'Tabulation' whereby each
-- entry in the given 'Tabulation' is wrapped in 'Rel8.justTable', and every
-- other possible key contains a single 'Rel8.nothingTable'.
--
-- This is used to implement 'leftAlignWith'.
optionalTabulation :: Tabulation k a -> Tabulation k (MaybeTable Expr a)
optionalTabulation (Tabulation f) = Tabulation $ \p -> case p of
  Predicate Nothing -> fmap pure <$> f p
  _ -> fmap (\m -> (empty, snd <$> m)) $ optional $ do
    (k, a) <- f p
    ensure p k
    pure (k, a)


-- | 'manyTabulation' aggregates each entry with a particular key into a
-- single entry with all of the values contained in a 'ListTable'.
--
-- 'orderTabulation' can be used to give this 'ListTable' a defined order.
--
-- The resulting 'Tabulation' is \"magic\" in that the value
-- @'Rel8.listTable []'@ exists at every possible key that wasn't in the given
-- 'Tabulation'.
manyTabulation :: (EqTable k, Table Expr a)
  => Tabulation k a -> Tabulation k (ListTable Expr a)
manyTabulation =
  fmap (maybeTable mempty (\(ListTable a) -> ListTable a)) .
  optionalTabulation .
  aggregateTabulation .
  fmap (listAgg . toCols)


-- | 'someTabulation' aggregates each entry with a particular key into a
-- single entry with all of the values contained in a 'NonEmptyTable'.
--
-- 'orderTabulation' can be used to give this 'NonEmptyTable' a defined order.
someTabulation :: (EqTable k, Table Expr a)
  => Tabulation k a -> Tabulation k (NonEmptyTable Expr a)
someTabulation =
  fmap (\(NonEmptyTable a) -> NonEmptyTable a) .
  aggregateTabulation .
  fmap (nonEmptyAgg . toCols)


-- | 'existsTabulation' produces a \"magic\" 'Tabulation' which contains the
-- value 'Rel8.true' at each key in the given 'Tabulation', and the value
-- 'Rel8.false' at every other possible key.
existsTabulation :: Tabulation k a -> Tabulation k (Expr Bool)
existsTabulation (Tabulation f) = Tabulation $ \p -> case p of
  Predicate Nothing -> (true <$) <$> f p
  _ -> fmap (empty,) $ exists $ do
    (k, _) <- f p
    ensure p k


-- | 'presentTabulation' produces a 'Tabulation' where a single @()@ row
-- exists for every key that was present in the given 'Tabulation'.
--
-- This is used to implement 'similarity'.
presentTabulation :: Tabulation k a -> Tabulation k ()
presentTabulation (Tabulation f) = Tabulation $ \p -> do
  present $ do
    (k, _) <- f p
    ensure p k
  pure (empty, ())


-- | 'absentTabulation' produces a 'Tabulation' where a single @()@ row exists
-- at every possible key that absent from the given 'Tabulation'.
--
-- This is used to implement 'difference'.
absentTabulation :: Tabulation k a -> Tabulation k ()
absentTabulation (Tabulation f) = Tabulation $ \p -> do
  absent $ do
    (k, _) <- f p
    ensure p k
  pure (empty, ())


-- | Performs a @NATURAL FULL OUTER JOIN@ based on the common key columns.
--
-- Analogous to 'Data.Semialign.align'.
align :: EqTable k
  => Tabulation k a -> Tabulation k b -> Tabulation k (TheseTable Expr a b)
align = alignWith id


-- | Performs a @NATURAL FULL OUTER JOIN@ based on the common key columns.
--
-- Analogous to 'Data.Semialign.alignWith'.
alignWith :: EqTable k
  => (TheseTable Expr a b -> c)
  -> Tabulation k a -> Tabulation k b -> Tabulation k c
alignWith f (Tabulation as) (Tabulation bs) = Tabulation $ \p -> do
  tkab <- liftF2 (alignBy condition) as bs p
  let
    k = recover $ bimap fst fst tkab
    tab = bimap snd snd tkab
  pure (k, f tab)
  where
    condition (k, _) (k', _) = fromMaybe true (liftA2 (==:) k k')
    recover (TheseTable mma@(MaybeTable _ ma) mmb@(MaybeTable _ mb)) =
      case extract ma of
        Nothing -> extract mb
        Just a -> case extract mb of
          Nothing -> extract ma
          Just b -> case a <$ mma <|>: b <$ mmb of
            MaybeTable _ c -> pure (extract c)


-- | Performs a @NATURAL LEFT OUTER JOIN@ based on the common key columns.
--
-- Analogous to 'Data.Semialign.rpadZip'.
--
-- Note that you can achieve the same effect with 'optionalTabulation' and the
-- 'Applicative' instance for 'Tabulation', i.e., this is just
-- @\left right -> liftA2 (,) left (optionalTabulation right). You can also
-- use @do@-notation.
leftAlign :: EqTable k
  => Tabulation k a -> Tabulation k b -> Tabulation k (a, MaybeTable Expr b)
leftAlign = leftAlignWith (,)


-- | Performs a @NATURAL LEFT OUTER JOIN@ based on the common key columns.
--
-- Analogous to 'Data.Semialign.rpadZipWith'.
--
-- Note that you can achieve the same effect with 'optionalTabulation' and the
-- 'Applicative' instance for 'Tabulation', i.e., this is just
-- @\f left right -> liftA2 f left (optionalTabulation right). You can also
-- use @do@-notation.
leftAlignWith :: EqTable k
  => (a -> MaybeTable Expr b -> c)
  -> Tabulation k a -> Tabulation k b -> Tabulation k c
leftAlignWith f left right = liftA2 f left (optionalTabulation right)


-- | Performs a @NATURAL RIGHT OUTER JOIN@ based on the common key columns.
--
-- Analogous to 'Data.Semialign.lpadZip'.
--
-- Note that you can achieve the same effect with 'optionalTabulation' and the
-- 'Applicative' instance for 'Tabulation', i.e., this is just
-- @\left right -> liftA2 (flip (,)) right (optionalTabulation left). You can
-- also use @do@-notation.
rightAlign :: EqTable k
  => Tabulation k a -> Tabulation k b -> Tabulation k (MaybeTable Expr a, b)
rightAlign = rightAlignWith (,)


-- | Performs a @NATURAL RIGHT OUTER JOIN@ based on the common key columns.
--
-- Analogous to 'Data.Semialign.lpadZipWith'.
--
-- Note that you can achieve the same effect with 'optionalTabulation' and the
-- 'Applicative' instance for 'Tabulation', i.e., this is just
-- @\f left right -> liftA2 (flip f) right (optionalTabulation left). You can
-- also use @do@-notation.
rightAlignWith :: EqTable k
  => (MaybeTable Expr a -> b -> c)
  -> Tabulation k a -> Tabulation k b -> Tabulation k c
rightAlignWith f left right = liftA2 (flip f) right (optionalTabulation left)


-- | Performs a @NATURAL INNER JOIN@ based on the common key columns.
--
-- Analagous to 'Data.Semialign.zip'.
--
-- Note that you can achieve the same effect with the 'Applicative' instance
-- of 'Tabulation', i.e., this is just @'liftA2 (,)'@. You can also use
-- @do@-notation.
zip :: EqTable k
  => Tabulation k a -> Tabulation k b -> Tabulation k (a, b)
zip = zipWith (,)


-- | Performs a @NATURAL INNER JOIN@ based on the common key columns.
--
-- Analagous to 'Data.Semialign.zipWith'.
--
-- Note that you can achieve the same effect with the 'Applicative' instance
-- of 'Tabulation', i.e., this is just @'liftA2'@. You can also use
-- @do@-notation.
zipWith :: EqTable k
  => (a -> b -> c) -> Tabulation k a -> Tabulation k b -> Tabulation k c
zipWith = liftA2


-- | Performs a [@NATURAL SEMI JOIN@](https://en.wikipedia.org/wiki/Relational_algebra#Semijoin_%28%E2%8B%89%29%28%E2%8B%8A%29)
-- based on the common key columns.
--
-- The result is a subset of the left tabulation where only entries which have
-- a corresponding entry in the right tabulation are kept.
--
-- Note that you can achieve a similar effect with 'presentTabulation' and the
-- 'Applicative' instance of 'Tabulation', i.e., this is just
-- @\left right -> left <* presentTabulation right@. You can also use
-- @do@-notation.
similarity :: EqTable k => Tabulation k a -> Tabulation k b -> Tabulation k a
similarity a b = a <* presentTabulation b


-- | Performs a [@NATURAL ANTI JOIN@](https://en.wikipedia.org/wiki/Relational_algebra#Antijoin_%28%E2%96%B7%29)
-- based on the common key columns.
--
-- The result is a subset of the left tabulation where only entries which do
-- not have a corresponding entry in the right tabulation are kept.
--
-- Note that you can achieve a similar effect with 'absentTabulation' and the
-- 'Applicative' instance of 'Tabulation', i.e., this is just
-- @\left right -> left <* absentTabulation right@. You can also use
-- @do@-notation.
difference :: EqTable k => Tabulation k a -> Tabulation k b -> Tabulation k a
difference a b = a <* absentTabulation b
