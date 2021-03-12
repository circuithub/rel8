{-# language DataKinds #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.Functor
  ( HFunctor( hmap )
  )
where

-- base
import Data.Kind ( Constraint )
import Prelude ()

-- rel8
import Rel8.Schema.HTable.Context ( HKTable )


type HFunctor :: (HKTable -> HKTable) -> Constraint
class HFunctor h where
  hmap :: (forall ctx. t ctx -> u ctx) -> h t context -> h u context
