{-# language StandaloneKindSignatures #-}

module Rel8.Schema.Kind
  ( Context, Table
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


data X


type Context :: Type
type Context = X -> Type


type Table :: Type
type Table = Context -> Type
