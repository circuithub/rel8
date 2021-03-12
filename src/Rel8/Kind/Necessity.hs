{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Kind.Necessity
  ( Necessity( Optional, Required )
  , SNecessity( SOptional, SRequired )
  , KnownNecessity( necessitySing )
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()


type Necessity :: Type
data Necessity = Optional | Required


type SNecessity :: Necessity -> Type
data SNecessity necessity where
  SOptional :: SNecessity 'Optional
  SRequired :: SNecessity 'Required


type KnownNecessity :: Necessity -> Constraint
class KnownNecessity necessity where
  necessitySing :: SNecessity necessity


instance KnownNecessity 'Optional where
  necessitySing = SOptional


instance KnownNecessity 'Required where
  necessitySing = SRequired
