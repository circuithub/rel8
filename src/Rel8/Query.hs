{-# language GeneralizedNewtypeDeriving #-}

module Rel8.Query where

import Rel8.FromRow
import Rel8.MonadQuery


newtype Query a = Query ( IO a )
  deriving ( Functor, Applicative, Monad )


instance MonadQuery Query


-- | Run a @SELECT@ query, returning all rows.
select :: FromRow row haskell => Query row -> m [ haskell ]
select _ =
  undefined
