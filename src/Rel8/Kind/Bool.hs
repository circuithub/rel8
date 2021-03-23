{-# language DataKinds #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Kind.Bool
  ( Bool( False, True )
  , SBool ( SFalse, STrue )
  , KnownBool( boolSing )
  , IsList
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
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


type IsList :: Type -> Bool
type family IsList a where
  IsList [_] = 'True
  IsList (NonEmpty _) = 'True
  IsList _ = 'False
