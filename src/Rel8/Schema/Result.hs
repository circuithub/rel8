{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.Result
  ( Result
  ) where

-- base
import Prelude ()

-- rel8
import Rel8.Schema.Kind ( Context )


type Result :: Context
data Result a
