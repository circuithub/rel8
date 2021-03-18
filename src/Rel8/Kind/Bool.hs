{-# language DataKinds #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Kind.Bool
  ( Bool( False, True )
  , SBool ( SFalse, STrue )
  , KnownBool( boolSing )
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude


type SBool :: Bool -> Type
data SBool bool where
  SFalse :: SBool 'False
  STrue :: SBool 'True


type KnownBool :: Bool -> Constraint
class KnownBool bool where
  boolSing :: SBool bool


instance KnownBool 'False where
  boolSing = SFalse


instance KnownBool 'True where
  boolSing = STrue
