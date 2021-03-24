{-# language DataKinds #-}
{-# language PolyKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.FCF ( Exp, Eval ) where

-- base
import Data.Kind ( Type )
import Prelude ()


type Exp :: Type -> Type
type Exp e = e -> Type


type Eval :: Exp e -> e
type family Eval a
