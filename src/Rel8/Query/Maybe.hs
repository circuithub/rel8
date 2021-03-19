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
import Rel8.Expr.Opaleye ( unsafeToPrimExpr )
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
        condition = unsafeToPrimExpr true


catMaybeTable :: MaybeTable a -> Query a
catMaybeTable ma@(MaybeTable _ a) = do
  where_ $ isJustTable ma
  pure a


bindMaybeTable :: (a -> Query (MaybeTable b)) -> MaybeTable a -> Query (MaybeTable b)
bindMaybeTable query (MaybeTable input a) = do
  MaybeTable output b <- query a
  pure $ MaybeTable (input <> output) b


traverseMaybeTable :: (a -> Query b) -> MaybeTable a -> Query (MaybeTable b)
traverseMaybeTable query ma@(MaybeTable input _) = do
  MaybeTable output b <- optional (query =<< catMaybeTable ma)
  where_ $ output ==. input
  pure $ MaybeTable input b
