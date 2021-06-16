{-# language DataKinds #-}
{-# language FlexibleInstances #-}
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
import Rel8.Schema.Kind ( Context )
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Reify ( Reify )
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Write ( Write )


type SContext :: Context -> Type
data SContext context where
  SAggregate :: SContext Aggregate
  SExpr :: SContext Expr
  SName :: SContext Name
  SResult :: SContext Result
  SWrite :: SContext Write
  SReify :: SContext context -> SContext (Reify context)


type Reifiable :: Context -> Constraint
class Interpretation context => Reifiable context where
  contextSing :: SContext context


instance Reifiable Aggregate where
  contextSing = SAggregate


instance Reifiable Expr where
  contextSing = SExpr


instance Reifiable Name where
  contextSing = SName


instance Reifiable Result where
  contextSing = SResult


instance Reifiable Write where
  contextSing = SWrite


instance Reifiable context => Reifiable (Reify context) where
  contextSing = SReify contextSing


sReifiable :: SContext context -> Dict Reifiable context
sReifiable = \case
  SAggregate -> Dict
  SExpr -> Dict
  SName -> Dict
  SResult -> Dict
  SWrite -> Dict
  SReify context -> case sReifiable context of
    Dict -> Dict


sLabelable :: SContext context -> Dict Labelable context
sLabelable = \case
  SAggregate -> Dict
  SExpr -> Dict
  SName -> Dict
  SResult -> Dict
  SWrite -> Dict
  SReify context -> case sLabelable context of
    Dict -> Dict
