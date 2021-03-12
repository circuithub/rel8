{-# language DataKinds #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Kind.Emptiability
  ( Emptiability( Emptiable, NonEmptiable )
  , SEmptiability ( SEmptiable, SNonEmptiable )
  , KnownEmptiability( emptiabilitySing )
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()


type Emptiability :: Type
data Emptiability = Emptiable | NonEmptiable


type SEmptiability :: Emptiability -> Type
data SEmptiability emptiability where
  SEmptiable :: SEmptiability 'Emptiable
  SNonEmptiable :: SEmptiability 'NonEmptiable


type KnownEmptiability :: Emptiability -> Constraint
class KnownEmptiability emptiability where
  emptiabilitySing :: SEmptiability emptiability


instance KnownEmptiability 'Emptiable where
  emptiabilitySing = SEmptiable


instance KnownEmptiability 'NonEmptiable where
  emptiabilitySing = SNonEmptiable
