{-# language DataKinds #-}
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

-- rel8
import Rel8.Kind.Nullability ( Nullability )


type role Expr representational representational
type Expr :: Nullability -> Type -> Type
newtype Expr nullability a = Expr Opaleye.PrimExpr
