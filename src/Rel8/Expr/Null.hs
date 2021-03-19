{-# options_ghc -Wno-orphans #-}

module Rel8.Expr.Null
  ( null
  , isNull
  , nullExpr
  , liftNull
  , mapNull
  , catMaybe
  , fromNull
  ) where

-- base
import Prelude ( Bool( False ), Maybe( Nothing ), ($), (.), id, return )

-- rel8
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.DBType ( DBType )
import Rel8.DBType.DBEq ( DBEq( (==.) ) )
import Rel8.Expr ( Expr, unsafeCoerceExpr, unsafeCoerceExpr )
import Rel8.Expr.Bool ( not_ )
import Rel8.Expr.Opaleye ( litExpr, mapPrimExpr )
import Rel8.Info ( HasInfo )
import Rel8.Query ( Query, where_ )
import Rel8.Serializable ( lit )
import Rel8.Table.Bool ( ifThenElse_ )


-- | Like 'maybe', but to eliminate @null@.
--
-- >>> select c $ pure $ null 0 id (nullExpr :: Expr (Maybe Int32))
-- [0]
--
-- >>> select c $ pure $ null 0 id (lit (Just 42) :: Expr (Maybe Int32))
-- [42]
null :: HasInfo b => Expr b -> (Expr a -> Expr b) -> Expr (Maybe a) -> Expr b
null whenNull f a = ifThenElse_ (isNull a) whenNull (f (unsafeCoerceExpr a))


-- | Like 'isNothing', but for @null@.
--
-- >>> select c $ pure $ isNull (nullExpr :: Expr (Maybe Int32))
-- [True]
--
-- >>> select c $ pure $ isNull (lit (Just 42) :: Expr (Maybe Int32))
-- [False]
isNull :: Expr (Maybe a) -> Expr Bool
isNull = mapPrimExpr ( Opaleye.UnExpr Opaleye.OpIsNull )


-- | Corresponds to SQL @null@.
nullExpr :: DBType a => Expr (Maybe a)
nullExpr = lit Nothing


-- | Lift an expression that's not null to a type that might be @null@. This is
-- an identity operation in terms of any generated query, and just modifies the
-- query's type.
liftNull :: Expr a -> Expr (Maybe a)
liftNull = unsafeCoerceExpr


-- | Lift an operation on non-@null@ values to an operation on possibly @null@
-- values.
-- 
-- @mapNull@ requires that the supplied function "preserves nulls", as no
-- actual case analysis is done (instead the @Expr (Maybe a)@ is simply unsafeCoerceExprd
-- and assumed to not be @null@). In most cases, this is true, but this
-- contract can be violated with custom functions.
mapNull :: (Expr a -> Expr b) -> Expr (Maybe a) -> Expr (Maybe b)
mapNull f = unsafeCoerceExpr . f . unsafeCoerceExpr


fromNull :: HasInfo a => Expr a -> Expr (Maybe a) -> Expr a
fromNull x = null x id


-- | Filter a 'Query' that might return @null@ to a 'Query' without any
-- @null@s.
--
-- Corresponds to 'Data.Maybe.catMaybes'.
-- 
-- >>> select c $ pure (nullExpr :: Expr (Maybe Bool))
-- [Nothing]
-- 
-- >>> select c $ catMaybe (nullExpr :: Expr (Maybe Bool))
-- []
-- 
-- >>> select c $ catMaybe (lit (Just True))
-- [True]
-- 
-- Notice how in the last example a @Bool@ is returned (rather than @Maybe
-- Bool@):
-- 
-- >>> :t catMaybe (lit (Just True))
-- catMaybe (lit (Just True)) :: Query (Expr Bool)
catMaybe :: Expr (Maybe a) -> Query (Expr a)
catMaybe e = do
  where_ $ not_ $ isNull e
  return $ unsafeCoerceExpr e


instance (DBType a, DBEq a) => DBEq (Maybe a) where
  a ==. b =
    null ( isNull b ) ( \a' -> null ( litExpr False ) ( a' ==. ) b ) a
