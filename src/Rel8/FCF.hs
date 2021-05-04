{-# language DataKinds #-}
{-# language PolyKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.FCF
  ( Exp, Eval
  , Compose
  )
where

-- base
import Data.Kind ( Type )
import Prelude ()


type Exp :: Type -> Type
type Exp e = e -> Type


type Eval :: Exp e -> e
type family Eval a


data Compose :: (Type -> Type) -> (Type -> Exp Type) -> Type -> Exp Type
type instance Eval (Compose f g a) = f (Eval (g a))
