{-# language ApplicativeDo #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.MaybeTable ( MaybeTable(..), toMaybe ) where

import Data.Functor.Identity
import Rel8.Column


{-| @MaybeTable t@ is the table @t@, but as the result of an outer join. If the
outer join fails to match any rows, this is essentialy @Nothing@, and if the
outer join does match rows, this is like @Just@.

-}
data MaybeTable t f =
  MaybeTable
    { -- | Check if this @MaybeTable@ is null. In other words, check if an outer
      -- join matched any rows.
      isNull :: Column f Bool
    , maybeTable :: t
    }


-- | If you 'select' a @MaybeTable@, you'll get back a @MaybeTable@ as a result.
-- However, this structure is awkward to use in ordinary Haskell, so 'toMaybe'
-- lets you transform a @MaybeTable@ into a normal @Maybe@ value.
toMaybe :: MaybeTable t Identity -> Maybe t
toMaybe MaybeTable{ isNull, maybeTable }
  | isNull = Nothing
  | otherwise = Just maybeTable
