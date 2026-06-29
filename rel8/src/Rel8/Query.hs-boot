{-# language StandaloneKindSignatures #-}

module Rel8.Query
  ( Query( Query )
  )
where

-- base
import Data.Kind ( Type )
import Data.Monoid ( Any )
import Prelude ()

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Select as Opaleye


type Query :: Type -> Type
newtype Query a = Query ([Opaleye.PrimExpr] -> Opaleye.Select (Any, a))
