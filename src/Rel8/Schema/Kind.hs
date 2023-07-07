{-# LANGUAGE StandaloneKindSignatures #-}

module Rel8.Schema.Kind (
  Rel8able,
  Context,
  HTable,
)
where

-- base
import Data.Kind (Type)
import Prelude ()


type Context :: Type
type Context = Type -> Type


type HTable :: Type
type HTable = Context -> Type


type Rel8able :: Type
type Rel8able = Context -> Type
