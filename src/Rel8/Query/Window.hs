module Rel8.Query.Window
  ( window
  )
where

-- base
import Prelude ()

-- opaleye
import qualified Opaleye.Window as Opaleye

-- rel8
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( mapOpaleye )
import Rel8.Window ( Window( Window ) )


-- | 'window' runs a query composed of expressions containing
-- [window functions](https://www.postgresql.org/docs/current/tutorial-window.html).
-- 'window' is similar to 'Rel8.aggregate', with the main difference being
-- that in a window query, each input row corresponds to one output row,
-- whereas aggregation queries fold the entire input query down into a single
-- row. To put this into a Haskell context, 'Rel8.aggregate' is to 'foldl' as
-- 'window' is to 'scanl'.
window :: Window a b -> Query a -> Query b
window (Window a) = mapOpaleye (Opaleye.runWindows a)
