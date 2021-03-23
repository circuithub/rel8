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
import {-# SOURCE #-} Rel8.Query ( Query )
import Rel8.Query.Opaleye ( zipOpaleyeWith )
import Rel8.Table ( Table  )
import Rel8.Table.Eq ( EqTable )
import Rel8.Table.Opaleye ( binaryspec )
import Rel8.Schema.Context ( DB )


union :: EqTable a => Query a -> Query a -> Query a
union = zipOpaleyeWith (Opaleye.unionExplicit binaryspec)


unionAll :: Table DB a => Query a -> Query a -> Query a
unionAll = zipOpaleyeWith (Opaleye.unionAllExplicit binaryspec)


intersect :: EqTable a => Query a -> Query a -> Query a
intersect = zipOpaleyeWith (Opaleye.intersectExplicit binaryspec)


intersectAll :: Table DB a => Query a -> Query a -> Query a
intersectAll = zipOpaleyeWith (Opaleye.intersectAllExplicit binaryspec)


except :: EqTable a => Query a -> Query a -> Query a
except = zipOpaleyeWith (Opaleye.exceptExplicit binaryspec)


exceptAll :: Table DB a => Query a -> Query a -> Query a
exceptAll = zipOpaleyeWith (Opaleye.exceptAllExplicit binaryspec)
