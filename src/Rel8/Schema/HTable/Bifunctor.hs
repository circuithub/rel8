{-# language DataKinds #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.Bifunctor
  ( HBifunctor( hbimap )
  )
where

-- base
import Data.Kind ( Constraint )
import Prelude ()

-- rel8
import Rel8.Schema.HTable.Context ( HKTable )


type HBifunctor :: (HKTable -> HKTable -> HKTable) -> Constraint
class HBifunctor h where
  hbimap :: (forall ctx. t ctx -> v ctx) -> (forall ctx. u ctx -> w ctx) -> h t u context -> h v w context
