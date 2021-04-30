{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Context
  ( Interpretation( Col )
  )
where

-- base
import Data.Kind ( Constraint )
import Prelude ()

-- rel8
import Rel8.Schema.Kind ( Context, HContext )


type Interpretation :: Context -> Constraint
class Interpretation context where
  data Col context :: HContext
