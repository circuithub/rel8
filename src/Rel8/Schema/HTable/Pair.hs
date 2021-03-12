{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.Pair
  ( HPair(..)
  )
where

-- base
import Prelude

-- rel8
import Rel8.Schema.HTable.Context ( HKTable )
import Rel8.Schema.HTable.Bifunctor ( HBifunctor( hbimap ) )
import Rel8.Schema.HTable.Functor ( HFunctor( hmap ) )


type HPair :: HKTable -> HKTable -> HKTable
data HPair fst snd context = HPair
  { hfst :: fst context
  , hsnd :: snd context
  }


instance HBifunctor HPair where
  hbimap f g (HPair a b) = HPair (f a) (g b)


instance HFunctor (HPair a) where
  hmap = hbimap id
