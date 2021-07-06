{-# language DataKinds #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Kind.Context
  ( Reifiable( contextSing )
  , SContext(..)
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Schema.Kind ( HContext )
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Result ( Result )


type SContext :: HContext -> Type
data SContext context where
  SAggregate :: SContext Aggregate
  SExpr :: SContext Expr
  SName :: SContext Name
  SResult :: SContext Result


type Reifiable :: HContext -> Constraint
class Reifiable context where
  contextSing :: SContext context


instance Reifiable Aggregate where
  contextSing = SAggregate


instance Reifiable Expr where
  contextSing = SExpr


instance Reifiable Result where
  contextSing = SResult


instance Reifiable Name where
  contextSing = SName
