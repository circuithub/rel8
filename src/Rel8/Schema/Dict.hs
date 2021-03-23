{-# language ConstraintKinds #-}
{-# language GADTs #-}
{-# language PolyKinds #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.Dict
  ( Dict( Dict )
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()


type Dict :: (a -> Constraint) -> a -> Type
data Dict c a where
  Dict :: c a => Dict c a
