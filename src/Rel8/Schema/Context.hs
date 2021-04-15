{-# language DataKinds #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Context
  ( Interpretation(..), Col( Result, K, unK, unResult )
  )
where

-- base
import Control.Applicative ( Const )
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint )
import Prelude ()

-- rel8
import Rel8.Schema.Kind ( Context, HContext )
import Rel8.Schema.Spec ( Spec( Spec ) )


type Interpretation :: Context -> Constraint
class Interpretation context where
  data Col context :: HContext


instance Interpretation Identity where
  data Col Identity _spec where
    Result :: { unResult :: a } -> Col Identity ('Spec labels necessity a)


instance Interpretation (Const x) where
  data Col (Const x) _spec where
    K :: { unK :: x } ->  Col (Const x) _spec
