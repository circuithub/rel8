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
import {-# SOURCE #-} Rel8.Query ( Query )
import Rel8.Query.Opaleye ( fromOpaleye )
import Rel8.Table ( Table )
import Rel8.Table.Opaleye ( valuesspec )
import Rel8.Schema.Context ( DB )


-- | Construct a query that returns the given input list of rows. This is like
-- folding a list of 'return' statements under 'union', but uses the SQL
-- @VALUES@ expression for efficiency.
--
-- Typically @values@ will be used with 'lit':
--
-- >>> mapM_ Data.Text.IO.putStrLn =<< select c (values [ lit "Hello", lit "World!" ])
-- Hello
-- World!
values :: (Table DB a, Foldable f) => f a -> Query a
values = fromOpaleye . Opaleye.valuesExplicit valuesspec . toList
