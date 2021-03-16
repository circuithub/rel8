{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Spec.ConstrainDBType
  ( ConstrainDBType
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Kind.Blueprint ( ToDBType )
import Rel8.Schema.Spec ( Spec( Spec ) )


type ConstrainDBType :: (Type -> Constraint) -> Spec -> Constraint
class
  ( forall labels necessity nullability blueprint a. ()
     => (spec ~ 'Spec labels necessity nullability blueprint, a ~ ToDBType blueprint)
     => constraint a
  ) =>
  ConstrainDBType constraint spec
instance
  ( spec ~ 'Spec labels necessity nullability blueprint
  , a ~ ToDBType blueprint
  , constraint a
  ) =>
  ConstrainDBType constraint spec
