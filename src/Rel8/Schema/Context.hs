{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Context
  ( Interpretation( Col )
  , UnCol
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


type UnCol :: HContext -> Context
type family UnCol context where
  UnCol (Col context) = context
