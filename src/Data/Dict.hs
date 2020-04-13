{-# language ConstraintKinds #-}
{-# language GADTs #-}
{-# language PolyKinds #-}

module Data.Dict where

import Data.Kind ( Constraint, Type )


data Dict :: Constraint -> Type where
  Dict :: c => Dict c
