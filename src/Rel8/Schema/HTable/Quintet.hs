{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.Quintet
  ( HQuintet(..)
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


type HQuintet :: HKTable -> HKTable -> HKTable -> HKTable -> HKTable -> HKTable
data HQuintet v w x y z context = HQuintet
  { hfst :: v context
  , hsnd :: w context
  , htrd :: x context
  , hfrt :: y context
  , hfft :: z context
  }
  deriving stock Generic
  deriving anyclass HTable


instance HBifunctor (HQuintet a b c) where
  hbimap f g (HQuintet a b c d e) = HQuintet a b c (f d) (g e)


instance HFunctor (HQuintet a b c d) where
  hmap = hbimap id
