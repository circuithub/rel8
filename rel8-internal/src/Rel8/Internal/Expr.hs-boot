{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}

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
newtype Expr a = Expr Opaleye.PrimExpr
