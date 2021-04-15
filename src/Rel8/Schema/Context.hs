{-# language DataKinds #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Context
  ( Interpretation(..), Col( Result )
  )
where

-- base
import Data.Kind ( Constraint )
import Prelude ()

-- rel8
import Rel8.Schema.Kind ( Context, HContext )
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Spec ( Spec( Spec ) )


type Interpretation :: Context -> Constraint
class Interpretation context where
  data Col context :: HContext


instance Interpretation Result where
  data Col Result _spec where
    Result :: a -> Col Result ('Spec labels necessity a)
