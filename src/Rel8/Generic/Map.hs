{-# language DataKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Generic.Map
  ( GMap
  , Map
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


-- | Map a @Type -> Type@ function over the @Type@-kinded type variables in
-- of a type constructor.
type Map :: (Type -> Exp Type) -> Type -> Type
type family Map f a where
  Map p (t a b c d e f g) =
    t (Eval (p a)) (Eval (p b)) (Eval (p c)) (Eval (p d)) (Eval (p e))
      (Eval (p f)) (Eval (p g))
  Map p (t a b c d e f) =
    t (Eval (p a)) (Eval (p b)) (Eval (p c)) (Eval (p d)) (Eval (p e))
      (Eval (p f))
  Map p (t a b c d e) =
    t (Eval (p a)) (Eval (p b)) (Eval (p c)) (Eval (p d)) (Eval (p e))
  Map p (t a b c d) = t (Eval (p a)) (Eval (p b)) (Eval (p c)) (Eval (p d))
  Map p (t a b c) = t (Eval (p a)) (Eval (p b)) (Eval (p c))
  Map p (t a b) = t (Eval (p a)) (Eval (p b))
  Map p (t a) = t (Eval (p a))
