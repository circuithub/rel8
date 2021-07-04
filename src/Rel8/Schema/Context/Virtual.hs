{-# language EmptyCase #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Context.Virtual
  ( Virtual, virtual
  , Abstract(..), Concrete(..), abstractOrConcrete, absurd
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Context ( SContext(..) )
import Rel8.Schema.Field ( Field )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Result ( Result )


type Virtual :: K.Context -> Constraint
class Virtual context where
  virtual :: Abstract context


instance Virtual Aggregate where
  virtual = VAggregate


instance Virtual Expr where
  virtual = VExpr


instance Virtual (Field table) where
  virtual = VField


instance Virtual Name where
  virtual = VName


type Abstract :: K.Context -> Type
data Abstract context where
  VAggregate :: Abstract Aggregate
  VExpr :: Abstract Expr
  VField :: Abstract (Field table)
  VName :: Abstract Name


type Concrete :: K.Context -> Type
data Concrete context where
  VResult :: Concrete Result


abstractOrConcrete :: ()
  => SContext context
  -> Either (Concrete context) (Abstract context)
abstractOrConcrete = \case
  SAggregate -> Right VAggregate
  SExpr -> Right VExpr
  SField -> Right VField
  SName -> Right VName
  SResult -> Left VResult


absurd :: Abstract context -> Concrete context -> a
absurd = \case
  VAggregate -> \case
  VExpr -> \case
  VField -> \case
  VName -> \case
