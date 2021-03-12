{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DataKinds #-}
{-# language NamedFieldPuns #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.HTable.These
  ( HTheseTable(..)
  )
where

-- base
import GHC.Generics ( Generic )
import Prelude

-- rel8
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Context ( HKTable )
import Rel8.Schema.HTable.Bifunctor ( HBifunctor, hbimap )
import Rel8.Schema.HTable.Functor ( HFunctor, hmap )
import Rel8.Schema.HTable.Maybe ( HMaybeTable  )


type HTheseTable :: HKTable -> HKTable -> HKTable
data HTheseTable a b context = HTheseTable
  { hhere :: HMaybeTable a context
  , hthere :: HMaybeTable b context
  }
  deriving stock Generic
  deriving anyclass HTable


instance HBifunctor HTheseTable where
  hbimap f g HTheseTable {hhere, hthere} = HTheseTable
    { hhere = hmap f hhere
    , hthere = hmap g hthere
    }


instance HFunctor (HTheseTable here) where
  hmap = hbimap id
