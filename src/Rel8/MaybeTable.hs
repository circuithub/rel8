{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language UndecidableInstances #-}

module Rel8.MaybeTable ( MaybeTable(..) ) where

import Rel8.Expr
import Rel8.FromRow
import Rel8.Rewrite


data MaybeTable t =
  MaybeTable
    { isNull :: forall m. Expr m Bool
    , maybeTable :: t
    }


instance Rewrite f g a b => Rewrite f g ( MaybeTable a ) ( MaybeTable b ) where
  rewrite f MaybeTable{..} =
    MaybeTable { isNull, maybeTable = rewrite f maybeTable }


-- | A @MaybeTable@ can be selected into a Haskell 'Maybe'. If the table is null
-- (e.g., a @LEFT JOIN@ selected no rows) the result is 'Nothing', otherwise the
-- result is 'Just'.
instance FromRow sql haskell => FromRow ( MaybeTable sql ) ( Maybe haskell )
