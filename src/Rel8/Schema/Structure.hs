{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.Structure
  ( Structure
  )
where

-- base
import Prelude ()

-- rel8
import Rel8.Schema.Kind ( Context )


type Structure :: Context
data Structure a
