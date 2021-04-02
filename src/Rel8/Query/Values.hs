{-# language FlexibleContexts #-}

module Rel8.Query.Values
  ( values
  )
where

-- base
import Data.Foldable ( toList )
import Prelude

-- opaleye
import qualified Opaleye.Values as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import {-# SOURCE #-} Rel8.Query ( Query )
import Rel8.Query.Opaleye ( fromOpaleye )
import Rel8.Table ( Table )
import Rel8.Table.Opaleye ( valuesspec )


-- | Construct a query that returns the given input list of rows. This is like
-- folding a list of 'return' statements under 'Rel8.union', but uses the SQL
-- @VALUES@ expression for efficiency.
values :: (Table Expr a, Foldable f) => f a -> Query a
values = fromOpaleye . Opaleye.valuesExplicit valuesspec . toList
