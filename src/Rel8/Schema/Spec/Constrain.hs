{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Schema.Spec.Constrain
  ( ConstrainSpec
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Schema.Spec ( Spec( Spec ) )


type Unspec :: Spec -> Type
type family Unspec spec where
  Unspec ('Spec a) = a


type ConstrainSpec :: (Type -> Constraint) -> Spec -> Constraint
class (spec ~ 'Spec (Unspec spec), constraint (Unspec spec)) =>
  ConstrainSpec constraint spec
instance (spec ~ 'Spec (Unspec spec), constraint (Unspec spec)) =>
  ConstrainSpec constraint spec
