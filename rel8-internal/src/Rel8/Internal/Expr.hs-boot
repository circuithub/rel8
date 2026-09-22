{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language RoleAnnotations #-}

module Rel8.Internal.Expr
  ( Expr(..)
  )
where

-- base
import Prelude ()

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Internal.Schema.Kind ( Context )


type Expr :: Context
type role Expr nominal
newtype Expr a = Expr Opaleye.PrimExpr
