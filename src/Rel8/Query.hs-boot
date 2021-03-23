{-# language StandaloneKindSignatures #-}

module Rel8.Query
  ( Query( Query )
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()

-- opaleye
import qualified Opaleye.Select as Opaleye


type Query :: Type -> Type
newtype Query a = Query (Opaleye.Select a)
