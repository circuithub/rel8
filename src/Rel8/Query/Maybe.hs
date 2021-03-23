module Rel8.Query.Maybe
  ( optional
  , catMaybeTable
  , bindMaybeTable
  , traverseMaybeTable
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye

-- rel8
import Rel8.Expr.Bool ( true )
import Rel8.Expr.Eq ( (==.) )
import Rel8.Expr.Opaleye ( toPrimExpr )
import Rel8.Query ( Query )
import Rel8.Query.Filter ( where_ )
import Rel8.Query.Opaleye ( mapOpaleye )
import Rel8.Table.Maybe ( MaybeTable( MaybeTable ), isJustTable )
import Rel8.Table.Opaleye ( unpackspec )


optional :: Query a -> Query (MaybeTable a)
optional = mapOpaleye $ Opaleye.QueryArr . go
  where
    go query (i, left, tag) = (MaybeTable t' a, join, Opaleye.next tag')
      where
        (MaybeTable t a, right, tag') =
          Opaleye.runSimpleQueryArr (pure <$> query) (i, tag)
        (t', bindings) = Opaleye.run $
          Opaleye.runUnpackspec unpackspec (Opaleye.extractAttr "maybe" tag') t
        join = Opaleye.Join Opaleye.LeftJoin condition [] bindings left right
        condition = toPrimExpr true


catMaybeTable :: MaybeTable a -> Query a
catMaybeTable ma@(MaybeTable _ a) = do
  where_ $ isJustTable ma
  pure a


-- | @bindMaybeTable f x@ is similar to the monadic bind (@>>=@) operation. It
-- allows you to "extend" an optional query with another query. If either the
-- input or output are 'noTable', then the result is 'noTable'.
--
-- This is similar to 'traverseMaybeTable', followed by a @join@ on the
-- resulting @MaybeTable@s.
--
-- >>> select c $ bindMaybeTable (optional . values . pure . not_) =<< optional (values [lit True])
-- [Just False]
--
-- >>> select c $ bindMaybeTable (\_ -> return (noTable :: MaybeTable (Expr Bool))) =<< optional (values [lit True])
-- [Nothing]
--
-- >>> select c $ bindMaybeTable (optional . values . pure . not_) =<< return (noTable :: MaybeTable (Expr Bool))
-- [Nothing]
bindMaybeTable :: Monad m
  => (a -> m (MaybeTable b)) -> MaybeTable a -> m (MaybeTable b)
bindMaybeTable query (MaybeTable input a) = do
  MaybeTable output b <- query a
  pure $ MaybeTable (input <> output) b


-- | Extend an optional query with another query.  This is useful if you want
-- to step through multiple @LEFT JOINs@.
--
-- Note that @traverseMaybeTable@ takes a @a -> Query b@ function, which means
-- you also have the ability to "expand" one row into multiple rows.
--
-- >>> :{
-- duplicate :: Expr Bool -> Query (Expr Bool)
-- duplicate x = unionAll (return x) (return x)
-- :}
--
-- >>> select c $ traverseMaybeTable duplicate =<< optional (values [lit True])
-- [Just True,Just True]
--
-- Note that if the @a -> Query b@ function returns no rows, then the resulting
-- query will also have no rows:
--
-- >>> select c $ traverseMaybeTable (limit 0 . pure) =<< optional (values [lit True])
-- []
--
-- However, regardless of the given @a -> Query b@ function, if the input is
-- @noTable@, you will always get exactly one @noTable@ back:
--
-- >>> select c $ traverseMaybeTable duplicate (noTable :: MaybeTable (Expr Bool))
-- [Nothing]
--
-- >>> select c $ traverseMaybeTable (limit 0 . pure) (noTable :: MaybeTable (Expr Bool))
-- [Nothing]
traverseMaybeTable :: (a -> Query b) -> MaybeTable a -> Query (MaybeTable b)
traverseMaybeTable query ma@(MaybeTable input _) = do
  MaybeTable output b <- optional (query =<< catMaybeTable ma)
  where_ $ output ==. input
  pure $ MaybeTable input b
