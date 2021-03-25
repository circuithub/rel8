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
import Rel8.Schema.Nullability ( Sql )
import Rel8.Schema.Spec ( Spec( Spec ) )


type ConstrainDBType :: (Type -> Constraint) -> Spec -> Constraint
class
  ( forall c labels necessity a. ()
     => (spec ~ 'Spec labels necessity a)
     => (forall x. (constraint x => c x)) => Sql c a
  )
  => ConstrainDBType constraint spec
instance
  ( spec ~ 'Spec labels necessity a
  , Sql constraint a
  )
  => ConstrainDBType constraint spec
