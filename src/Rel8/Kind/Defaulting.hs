{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Kind.Defaulting
  ( Defaulting( HasDefault, NoDefault )
  , SDefaulting( SHasDefault, SNoDefault )
  , KnownDefaulting( defaultingSing )
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()


type Defaulting :: Type
data Defaulting = HasDefault | NoDefault


type SDefaulting :: Defaulting -> Type
data SDefaulting defaulting where
  SHasDefault :: SDefaulting 'HasDefault
  SNoDefault :: SDefaulting 'NoDefault


type KnownDefaulting :: Defaulting -> Constraint
class KnownDefaulting defaulting where
  defaultingSing :: SDefaulting defaulting


instance KnownDefaulting 'HasDefault where
  defaultingSing = SHasDefault


instance KnownDefaulting 'NoDefault where
  defaultingSing = SNoDefault
