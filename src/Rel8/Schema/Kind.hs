{-# language StandaloneKindSignatures #-}

module Rel8.Schema.Kind ( Rel8able, Context, HTable ) where

-- base
import Data.Kind ( Type )
import Prelude ()

-- rel8
import Rel8.Schema.Spec ( Spec )


type Context :: Type
type Context = Spec -> Type


type HTable :: Type
type HTable = Context -> Type


type Rel8able :: Type
type Rel8able = Context -> Type
