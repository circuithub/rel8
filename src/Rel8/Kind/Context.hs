{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Kind.Context
  ( Reifiable( contextSing )
  , SContext(..)
  , sReifiable
  , sLabelable
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.Context ( Interpretation )
import Rel8.Schema.Context.Label ( Labelable )
import Rel8.Schema.Insert ( Insert )
import Rel8.Schema.Kind ( Context )
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Reify ( Reify )
import Rel8.Schema.Result ( Result )


type SContext :: Context -> Type
data SContext context where
  SAggregate :: SContext Aggregate
  SExpr :: SContext Expr
  SInsert :: SContext Insert
  SName :: SContext Name
  SResult :: SContext Result
  SReify :: SContext context -> SContext (Reify context)


type Reifiable :: Context -> Constraint
class Interpretation context => Reifiable context where
  contextSing :: SContext context


instance Reifiable Aggregate where
  contextSing = SAggregate


instance Reifiable Expr where
  contextSing = SExpr


instance Reifiable Result where
  contextSing = SResult


instance Reifiable Insert where
  contextSing = SInsert


instance Reifiable Name where
  contextSing = SName


instance Reifiable context => Reifiable (Reify context) where
  contextSing = SReify contextSing


sReifiable :: SContext context -> Dict Reifiable context
sReifiable = \case
  SAggregate -> Dict
  SExpr -> Dict
  SInsert -> Dict
  SName -> Dict
  SResult -> Dict
  SReify context -> case sReifiable context of
    Dict -> Dict


sLabelable :: SContext context -> Dict Labelable context
sLabelable = \case
  SAggregate -> Dict
  SExpr -> Dict
  SInsert -> Dict
  SName -> Dict
  SResult -> Dict
  SReify context -> case sLabelable context of
    Dict -> Dict
