{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Spec.ConstrainType
  ( ConstrainType
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Schema.Spec ( Spec( Spec ) )


type ConstrainType :: (Type -> Constraint) -> Spec -> Constraint
class
  ( forall labels defaulting a. ()
     => (spec ~ 'Spec labels defaulting a)
     => constraint a
  ) =>
  ConstrainType constraint spec
instance (spec ~ 'Spec labels defaulting a, constraint a) =>
  ConstrainType constraint spec
