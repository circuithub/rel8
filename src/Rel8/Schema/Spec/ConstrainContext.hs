{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Schema.Spec.ConstrainContext
  ( ConstrainContext
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Schema.Spec ( Context, Spec )


type ConstrainContext :: (Type -> Constraint) -> Context -> Spec -> Constraint
class constraint (context spec) => ConstrainContext constraint context spec
instance constraint (context spec) => ConstrainContext constraint context spec
