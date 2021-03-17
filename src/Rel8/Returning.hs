{-# language GADTs #-}

module Rel8.Returning ( Returning(..) ) where

-- base
import Data.Int ( Int64 )

-- rel8
import Rel8.Serializable ( Serializable )
import Rel8.Table.Selects ( Selects )


-- | @Returning@ describes what information to return when an @INSERT@
-- statement completes.
data Returning schema a where
  -- | Just return the number of rows inserted.
  NumberOfRowsAffected :: Returning schema Int64

  -- | Return a projection of the rows inserted. This can be useful if your
  -- insert statement increments sequences by using default values.
  --
  -- >>> :t insert Insert{ returning = Projection fooId }
  -- IO [ FooId ]
  Projection
    :: ( Selects schema row, Serializable projection a )
    => (row -> projection)
    -> Returning schema [a]

