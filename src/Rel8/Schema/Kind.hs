{-# language StandaloneKindSignatures #-}

module Rel8.Schema.Kind ( Rel8able, Context, HContext, HTable ) where

-- base
import Data.Kind ( Type )
import Prelude ()

-- rel8
import Rel8.Schema.Spec ( Spec )


type Context :: Type
type Context = X -> Type


data X


type HContext :: Type
type HContext = Spec -> Type


type HTable :: Type
type HTable = HContext -> Type


type Rel8able :: Type
type Rel8able = HContext -> Type
