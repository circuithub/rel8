{-# language DataKinds #-}
{-# language PolyKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.FCF
  ( Exp, Eval
  , Id
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()


type Exp :: Type -> Type
type Exp e = e -> Type


type Eval :: Exp e -> e
type family Eval a


data Id :: a -> Exp a
type instance Eval (Id a) = a
