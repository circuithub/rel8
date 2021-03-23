{-# language RoleAnnotations #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Expr
  ( Expr(..)
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye


type role Expr representational
type Expr :: Type -> Type
newtype Expr a = Expr Opaleye.PrimExpr
