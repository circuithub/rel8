{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Generic.Reify
  ( ARep
  )
where

-- base
import Data.Functor.Contravariant ( Contravariant, contramap )
import Data.Kind ( Type )
import Data.Void ( Void, absurd )
import GHC.Generics ( Generic, Rep, from, to )
import Prelude


type ARep :: (Type -> Type) -> Type
newtype ARep rep = ARep (rep Void)


instance (Contravariant rep, Functor rep) => Generic (ARep rep) where
  type Rep (ARep rep) = rep
  from (ARep a) = fmap absurd a
  to = ARep . contramap absurd
