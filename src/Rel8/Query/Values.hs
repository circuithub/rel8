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


values :: (Table DB a, Foldable f) => f a -> Query a
values = fromOpaleye . Opaleye.valuesExplicit valuesspec . toList
