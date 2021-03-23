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


type Returning :: Type -> Type -> Type
data Returning names a where
  NumberOfRowsAffected :: Returning names Int64
  Projection :: (Selects names exprs, Serializable projection a)
    => (exprs -> projection)
    -> Returning names [a]
