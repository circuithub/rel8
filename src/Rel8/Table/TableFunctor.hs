{-# language FlexibleContexts #-}

module Rel8.Table.TableFunctor ( TableFunctor(..) ) where

import Rel8.Schema.FieldName
import Rel8.Table.Recontextualize

-- | The class of types that act as functors on 'Table's.
class TableFunctor f where
  -- | Re-project the columns contained within a table functor. This is similar
  -- to 'fmap' for ordinary Haskell functors, but in 'tmap' you can only
  -- restrict the selection of columns (or re-order).
  tmap :: (Recontextualize g (FieldName a) a a', Recontextualize (FieldName a) g b' b) => (a' -> b') -> f a -> f b
