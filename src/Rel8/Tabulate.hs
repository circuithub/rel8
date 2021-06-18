{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TupleSections #-}
{-# language UndecidableInstances #-}

module Rel8.Tabulate
  ( Tabulation

  , fromQuery
  , toQuery
  , liftQuery
  , prebind
  , postbind
  , lookup

  , aggregateTabulation
  , distinctTabulation
  , orderTabulation
  , countTabulation

  , optionalTabulation
  , manyTabulation
  , someTabulation

  , existsTabulation
  , presentTabulation
  , absentTabulation

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
import Rel8.Table.Eq ( EqTable, (==:), eqTable )
import Rel8.Table.List ( ListTable( ListTable ) )
import Rel8.Table.Maybe ( MaybeTable( MaybeTable ), maybeTable )
import Rel8.Table.NonEmpty ( NonEmptyTable( NonEmptyTable ) )
import Rel8.Table.Opaleye ( aggregator, unpackspec )
import Rel8.Table.Ord ( OrdTable )
import Rel8.Table.Order ( ascTable )
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


type Tabulation :: Type -> Type -> Type
newtype Tabulation k a = Tabulation (Predicate k -> Query (Key k, a))


instance Bifunctor Tabulation where
  bimap f g (Tabulation a) = Tabulation $ \p ->
    bimap (fmap f) g <$> a (f >$< p)


instance Functor (Tabulation k) where
  fmap = second


instance EqTable k => Apply (Tabulation k) where
  liftF2 = liftA2


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


instance EqTable k => AltTable (Tabulation k) where
  as <|>: bs = catNonEmptyTable `postbind` ((<>) `on` someTabulation) as bs


instance EqTable k => AlternativeTable (Tabulation k) where
  emptyTable = Tabulation $ const $ fmap (empty,) emptyTable


instance (EqTable k, Table Expr a, Semigroup a) => Semigroup (Tabulation k a)
 where
  (<>) = alignWith (theseTable id id (<>))


instance (EqTable k, Table Expr a, Semigroup a) => Monoid (Tabulation k a)
 where
  mempty = emptyTable


fromQuery :: Query (k, a) -> Tabulation k a
fromQuery = Tabulation . const . fmap (first pure)


toQuery :: Table Expr k => Tabulation k a -> Query (k, a)
toQuery (Tabulation f) = do
  (mk, a) <- f mempty
  k <- cat mk
  pure (k, a)


liftQuery :: Query a -> Tabulation k a
liftQuery = Tabulation . const . fmap (empty,)


prebind :: (a -> Tabulation k b) -> Query a -> Tabulation k b
prebind f as = Tabulation $ \p -> do
  a <- as
  case f a of
    Tabulation query -> query p
infixr 1 `prebind`


postbind :: (a -> Query b) -> Tabulation k a -> Tabulation k b
postbind f (Tabulation as) = Tabulation $ \p -> do
  (k, a) <- as p
  b <- f a
  pure (k, b)
infixr 1 `postbind`


lookup :: EqTable k => k -> Tabulation k a -> Query a
lookup k (Tabulation f) = do
  (k', a) <- f p
  ensure p k'
  pure a
  where
    p = match (pure k)


aggregateTabulation :: forall k aggregates exprs.
  ( EqTable k
  , Aggregates aggregates exprs
  )
  => Tabulation k aggregates -> Tabulation k exprs
aggregateTabulation (Tabulation f) = Tabulation $
  fmap (first (fmap fromColumns)) .
  mapOpaleye (Opaleye.aggregate (keyed aggregator aggregator)) .
  fmap (first (fmap (hgroupBy (eqTable @k) . toColumns))) .
  f


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


orderTabulation :: OrdTable k => Order a -> Tabulation k a -> Tabulation k a
orderTabulation ordering (Tabulation f) =
  Tabulation $ mapOpaleye (Opaleye.orderBy ordering') . f
  where
    Order ordering' = runClown (keyed (Clown ascTable) (Clown ordering))


countTabulation :: EqTable k => Tabulation k a -> Tabulation k (Expr Int64)
countTabulation =
  fmap (maybeTable 0 id) .
  optionalTabulation .
  aggregateTabulation .
  fmap (const countStar)


optionalTabulation :: Tabulation k a -> Tabulation k (MaybeTable a)
optionalTabulation (Tabulation f) = Tabulation $ \p -> case p of
  Predicate Nothing -> fmap pure <$> f p
  _ -> fmap (\m -> (empty, snd <$> m)) $ optional $ do
    (k, a) <- f p
    ensure p k
    pure (k, a)


manyTabulation :: (EqTable k, Table Expr a)
  => Tabulation k a -> Tabulation k (ListTable a)
manyTabulation =
  fmap (maybeTable mempty (\(ListTable a) -> ListTable a)) .
  optionalTabulation .
  aggregateTabulation .
  fmap (listAgg . toColumns)


someTabulation :: (EqTable k, Table Expr a)
  => Tabulation k a -> Tabulation k (NonEmptyTable a)
someTabulation =
  fmap (\(NonEmptyTable a) -> NonEmptyTable a) .
  aggregateTabulation .
  fmap (nonEmptyAgg . toColumns)


existsTabulation :: Tabulation k a -> Tabulation k (Expr Bool)
existsTabulation (Tabulation f) = Tabulation $ \p -> case p of
  Predicate Nothing -> (true <$) <$> f p
  _ -> fmap (empty,) $ exists $ do
    (k, _) <- f p
    ensure p k


presentTabulation :: Tabulation k a -> Tabulation k ()
presentTabulation (Tabulation f) = Tabulation $ \p -> do
  present $ do
    (k, _) <- f p
    ensure p k
  pure (empty, ())


absentTabulation :: Tabulation k a -> Tabulation k ()
absentTabulation (Tabulation f) = Tabulation $ \p -> do
  absent $ do
    (k, _) <- f p
    ensure p k
  pure (empty, ())


align :: EqTable k
  => Tabulation k a -> Tabulation k b -> Tabulation k (TheseTable a b)
align = alignWith id


alignWith :: EqTable k
  => (TheseTable a b -> c)
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
      case ma of
        Nothing -> mb
        Just a -> case mb of
          Nothing -> ma
          Just b -> case a <$ mma <|>: b <$ mmb of
            MaybeTable _ c -> pure c


leftAlign :: EqTable k
  => Tabulation k a -> Tabulation k b -> Tabulation k (a, MaybeTable b)
leftAlign = leftAlignWith (,)


leftAlignWith :: EqTable k
  => (a -> MaybeTable b -> c)
  -> Tabulation k a -> Tabulation k b -> Tabulation k c
leftAlignWith f left right = liftA2 f left (optionalTabulation right)


rightAlign :: EqTable k
  => Tabulation k a -> Tabulation k b -> Tabulation k (MaybeTable a, b)
rightAlign = rightAlignWith (,)


rightAlignWith :: EqTable k
  => (MaybeTable a -> b -> c)
  -> Tabulation k a -> Tabulation k b -> Tabulation k c
rightAlignWith f left right = liftA2 (flip f) right (optionalTabulation left)


zip :: EqTable k
  => Tabulation k a -> Tabulation k b -> Tabulation k (a, b)
zip = zipWith (,)


zipWith :: EqTable k
  => (a -> b -> c) -> Tabulation k a -> Tabulation k b -> Tabulation k c
zipWith = liftA2


similarity :: EqTable k => Tabulation k a -> Tabulation k b -> Tabulation k a
similarity a b = a <* presentTabulation b


difference :: EqTable k => Tabulation k a -> Tabulation k b -> Tabulation k a
difference a b = a <* absentTabulation b
