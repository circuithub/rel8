{-# language DataKinds #-}
{-# language GADTs #-}

module Rel8.Context ( KContext, Context ) where

-- base
import Data.Kind ( Type )


data KContext where
  Context :: (Type -> Type) -> KContext


type Context = 'Context
