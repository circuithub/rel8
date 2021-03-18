{-# language FlexibleContexts #-}

module Rel8.Query.These
  ( alignBy
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.Join as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( exprToColumn )
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( zipOpaleyeWith )
import Rel8.Schema.Context ( DB )
import Rel8.Table ( Table )
import Rel8.Table.Opaleye ( unpackspec )
import Rel8.Table.These ( TheseTable( TheseTable ) )


alignBy :: (Table DB a, Table DB b)
  => (a -> b -> Expr nullability Bool)
  -> Query a -> Query b -> Query (TheseTable a b)
alignBy condition as bs =
  uncurry TheseTable <$> zipOpaleyeWith fullOuterJoin as bs
  where
    fullOuterJoin a b =
      Opaleye.joinExplicit unpackspec unpackspec pure pure full a b on
      where
        full = Opaleye.FullJoin
        on = exprToColumn . uncurry condition
