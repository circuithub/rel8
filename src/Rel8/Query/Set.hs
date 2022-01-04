{-# language FlexibleContexts #-}

module Rel8.Query.Set
  ( union, unionAll
  , intersect, intersectAll
  , except, exceptAll
  )
where

-- base
import Prelude ()

-- opaleye
import qualified Opaleye.Binary as Opaleye

-- rel8
import Rel8.Expr ( Expr )
import {-# SOURCE #-} Rel8.Query ( Query )
import Rel8.Query.Opaleye ( zipOpaleyeWith )
import Rel8.Table ( Table  )
import Rel8.Table.Eq ( EqTable )
import Rel8.Table.Opaleye ( binaryspec )


-- | Combine the results of two queries of the same type, collapsing
-- duplicates.  @union a b@ is the same as the SQL statement @x UNION b@.
union :: EqTable a => Query a -> Query a -> Query a
union = zipOpaleyeWith (Opaleye.unionExplicit binaryspec)


-- | Combine the results of two queries of the same type, retaining duplicates.
-- @unionAll a b@ is the same as the SQL statement @x UNION ALL b@.
unionAll :: Table Expr a => Query a -> Query a -> Query a
unionAll = zipOpaleyeWith (Opaleye.unionAllExplicit binaryspec)


-- | Find the intersection of two queries, collapsing duplicates.  @intersect a
-- b@ is the same as the SQL statement @x INTERSECT b@.
intersect :: EqTable a => Query a -> Query a -> Query a
intersect = zipOpaleyeWith (Opaleye.intersectExplicit binaryspec)


-- | Find the intersection of two queries, retaining duplicates.  @intersectAll
-- a b@ is the same as the SQL statement @x INTERSECT ALL b@.
intersectAll :: EqTable a => Query a -> Query a -> Query a
intersectAll = zipOpaleyeWith (Opaleye.intersectAllExplicit binaryspec)


-- | Find the difference of two queries, collapsing duplicates @except a b@ is
-- the same as the SQL statement @x EXCEPT b@.
except :: EqTable a => Query a -> Query a -> Query a
except = zipOpaleyeWith (Opaleye.exceptExplicit binaryspec)


-- | Find the difference of two queries, retaining duplicates.  @exceptAll a b@
-- is the same as the SQL statement @x EXCEPT ALL b@.
exceptAll :: EqTable a => Query a -> Query a -> Query a
exceptAll = zipOpaleyeWith (Opaleye.exceptAllExplicit binaryspec)
