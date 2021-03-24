{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Statement.Returning
  ( Returning(..)
  )
where

-- base
import Data.Int ( Int64 )
import Data.Kind ( Type )
import Prelude ()

-- rel8
import Rel8.Table.Recontextualize ( Selects )
import Rel8.Table.Serialize ( Serializable )


-- | @INSERT@, @UPDATE@ and @DELETE@ all support returning either the number of
-- rows affected, or the actual rows modified. 'Projection' allows you to
-- project out of these returned rows, which can be useful if you want to log
-- exactly which rows were deleted, or to view a generated id (for example, if
-- using a column with an autoincrementing counter as a default value).
type Returning :: Type -> Type -> Type
data Returning names a where
  NumberOfRowsAffected :: Returning names Int64
  Projection :: (Selects names exprs, Serializable projection a)
    => (exprs -> projection)
    -> Returning names [a]
