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


tabulate :: (a -> k) -> a -> Tabulation k a
tabulate key a = fromQuery $ pure (key a, a)


tabulateA :: (a -> Query k) -> a -> Tabulation k a
tabulateA key a = fromQuery $ (,a) <$> key a


fromQuery :: Query (k, a) -> Tabulation k a
fromQuery = Tabulation . const . fmap (first Just)


indexed :: Tabulation k a -> Tabulation k (k, a)
indexed (Tabulation query) = Tabulation $ \i ->
  (\(mk, a) -> (mk, (fromMaybe i mk, a))) <$> query i


ifilter :: (k -> a -> Expr Bool) -> Tabulation k a -> Tabulation k a
ifilter f tabulation = snd <$> do
  filter (uncurry f) `postbind` indexed tabulation


prebind :: (a -> Tabulation k b) -> Query a -> Tabulation k b
prebind f as = Tabulation $ \k -> do
  a <- as
  case f a of
    Tabulation query -> query k
infixr 1 `prebind`


postbind :: (a -> Query b) -> Tabulation k a -> Tabulation k b
postbind f (Tabulation as) = Tabulation $ \i -> do
  (k, a) <- as i
  case f a of
    bs -> do
      b <- bs
      pure (k, b)
infixr 1 `postbind`


lookup :: EqTable k => k -> Tabulation k a -> Query a
lookup key (Tabulation query) = do
  (mk, a) <- query key
  traverse_ (where_ . (key ==:)) mk
  pure a


align :: (EqTable k, Table Expr a, Table Expr b)
  => Tabulation k a -> Tabulation k b -> Tabulation k (TheseTable a b)
align = alignWith id


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


leftAlign :: EqTable k
  => Tabulation k a -> Tabulation k b -> Tabulation k (a, MaybeTable b)
leftAlign = leftAlignWith (,)


leftAlignWith :: EqTable k
  => (a -> MaybeTable b -> c)
  -> Tabulation k a -> Tabulation k b -> Tabulation k c
leftAlignWith f left right = liftA2 f left (optionalTabulation right)


zip :: EqTable k
  => Tabulation k a -> Tabulation k b -> Tabulation k (a, b)
zip = zipWith (,)


zipWith :: EqTable k
  => (a -> b -> c) -> Tabulation k a -> Tabulation k b -> Tabulation k c
zipWith = liftA2


similarity :: EqTable k => Tabulation k a -> Tabulation k b -> Tabulation k a
similarity kas kbs = do
  as <- toQuery kas
  bs <- toQuery kbs
  fromQuery $ as >>= withBy (\(k, _) (l, _) -> k ==: l) bs


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
