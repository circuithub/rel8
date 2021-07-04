{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Rel8.Schema.Context.Abstract
  ( Abstract, virtual
  , Virtual(..), virtualOrResult, exclusivity
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.Type.Equality ( (:~:)( Refl) )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Context ( SContext(..) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Schema.Reify ( Reify )
import Rel8.Schema.Result ( Result, NotResult( NotResult ) )


type Abstract :: K.Context -> Constraint
type family Abstract context where
  Abstract (Reify context) = IsVirtual context
  Abstract _ = ()


type IsVirtual :: K.Context -> Constraint
class IsVirtual context where
  virtual :: Virtual context


instance IsVirtual Aggregate where
  virtual = VAggregate


instance IsVirtual Expr where
  virtual = VExpr


instance IsVirtual Name where
  virtual = VName


instance IsVirtual (Reify context) where
  virtual = VReify


type Virtual :: K.Context -> Type
data Virtual context where
  VAggregate :: Virtual Aggregate
  VExpr :: Virtual Expr
  VName :: Virtual Name
  VReify :: Virtual (Reify context)


virtualOrResult :: ()
  => SContext context
  -> Either (context :~: Result) (Virtual context)
virtualOrResult = \case
  SAggregate -> Right VAggregate
  SExpr -> Right VExpr
  SName -> Right VName
  SResult -> Left Refl
  SReify _ -> Right VReify


exclusivity :: Virtual context -> NotResult context
exclusivity = \case
  VAggregate -> NotResult
  VExpr -> NotResult
  VName -> NotResult
  VReify -> NotResult
