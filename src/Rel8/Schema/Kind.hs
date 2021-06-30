{-# language StandaloneKindSignatures #-}

module Rel8.Schema.Kind
  ( Context, Rel8able
  , HContext, HTable
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()

-- rel8
import Rel8.Schema.Spec ( Spec )


type HContext :: Type
type HContext = Spec -> Type


type HTable :: Type
type HTable = HContext -> Type


type Context :: Type
type Context = Type -> Type


type Rel8able :: Type
type Rel8able = Context -> Type
