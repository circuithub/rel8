{-# language ApplicativeDo #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.MaybeTable ( MaybeTable(..) ) where

import Rel8.Column


{-| @MaybeTable t@ is the table @t@, but as the result of an outer join. If the
outer join fails to match any rows, this is essentialy @Nothing@, and if the
outer join does match rows, this is like @Just@.

-}
data MaybeTable t f =
  MaybeTable
    { isNull :: Column f Bool
    , maybeTable :: t
    }
