{-# language DataKinds #-}
{-# language GADTs #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
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

import Rel8.Schema.Spec ( Spec(..) )


type Expr :: k -> Type
data Expr a where
  Expr :: forall (a :: Type). !Opaleye.PrimExpr -> Expr a
  E :: { unE :: !(Expr a) } -> Expr ('Spec a)
