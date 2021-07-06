{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Rel8.Generic.Map
  ( GMap
  )
where

-- base
import Data.Kind ( Type )
import GHC.Generics
  ( (:+:), (:*:), K1, M1, U1, V1
  )
import Prelude ()

-- rel8
import Rel8.FCF ( Eval, Exp )


type GMap :: (Type -> Exp Type) -> (Type -> Type) -> Type -> Type
type family GMap f rep where
  GMap f (M1 i c rep) = M1 i c (GMap f rep)
  GMap _ V1 = V1
  GMap f (rep1 :+: rep2) = GMap f rep1 :+: GMap f rep2
  GMap _ U1 = U1
  GMap f (rep1 :*: rep2) = GMap f rep1 :*: GMap f rep2
  GMap f (K1 i a) = K1 i (Eval (f a))
