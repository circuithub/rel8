{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Rel8.Kind.Algebra (
  Algebra (Product, Sum),
  SAlgebra (SProduct, SSum),
  KnownAlgebra (algebraSing),
)
where

-- base
import Data.Kind (Constraint, Type)
import Prelude ()


type Algebra :: Type
data Algebra = Product | Sum


type SAlgebra :: Algebra -> Type
data SAlgebra algebra where
  SProduct :: SAlgebra 'Product
  SSum :: SAlgebra 'Sum


type KnownAlgebra :: Algebra -> Constraint
class KnownAlgebra algebra where
  algebraSing :: SAlgebra algebra


instance KnownAlgebra 'Product where
  algebraSing = SProduct


instance KnownAlgebra 'Sum where
  algebraSing = SSum
