{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.Trio
  ( HTrio(..)
  )
where

-- base
import GHC.Generics ( Generic )
import Prelude

-- rel8
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Context ( HKTable )
import Rel8.Schema.HTable.Bifunctor ( HBifunctor( hbimap ) )
import Rel8.Schema.HTable.Functor ( HFunctor( hmap ) )


type HTrio :: HKTable -> HKTable -> HKTable -> HKTable
data HTrio x y z context = HTrio
  { hfst :: x context
  , hsnd :: y context
  , htrd :: z context
  }
  deriving stock Generic
  deriving anyclass HTable


instance HBifunctor (HTrio a) where
  hbimap f g (HTrio a b c) = HTrio a (f b) (g c)


instance HFunctor (HTrio a b) where
  hmap = hbimap id
