{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilyDependencies #-}

module Rel8.Schema.Context.Lower
  ( Lower
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()

-- rel8
import Rel8.Schema.Kind ( Context )


type Lower :: Context -> Type -> Type
type family Lower context = f | f -> context
