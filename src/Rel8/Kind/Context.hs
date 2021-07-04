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
import Rel8.Schema.Field ( Field )
import Rel8.Schema.Kind ( Context )
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Result ( Result )


type SContext :: Context -> Type
data SContext context where
  SAggregate :: SContext Aggregate
  SExpr :: SContext Expr
  SField :: SContext (Field table)
  SName :: SContext Name
  SResult :: SContext Result


type Reifiable :: Context -> Constraint
class Reifiable context where
  contextSing :: SContext context


instance Reifiable Aggregate where
  contextSing = SAggregate


instance Reifiable Expr where
  contextSing = SExpr


instance Reifiable (Field table) where
  contextSing = SField


instance Reifiable Result where
  contextSing = SResult


instance Reifiable Name where
  contextSing = SName
