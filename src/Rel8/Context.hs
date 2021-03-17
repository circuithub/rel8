{-# language DataKinds #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Context ( KContext(..), Context ) where

import Data.Kind ( Type )


type KContext :: Type
data KContext where
  Context :: (Type -> Type) -> KContext


type Context :: (Type -> Type) -> KContext
type Context = 'Context
