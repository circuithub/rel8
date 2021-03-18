module Rel8.Query.Maybe
  ( optional
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
import Rel8.Expr.Opaleye ( unsafeToPrimExpr )
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( mapOpaleye )
import Rel8.Table.Maybe ( MaybeTable( MaybeTable ) )
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
