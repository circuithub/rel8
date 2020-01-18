{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.HigherKinded where

import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import GHC.Exts ( Constraint )
import Rel8.Column ( C )
import Rel8.Unconstrained ( Unconstrained )


{-| The class of higher-kinded data types.

Higher-kinded data types are data types of the pattern:

@
data MyType f =
  MyType { field1 :: Column f T1 OR HK1 f
         , field2 :: Column f T2 OR HK2 f
         , ...
         , fieldN :: Column f Tn OR HKn f
         }
@

where @Tn@ is any Haskell type, and @HKn@ is any higher-kinded type.

That is, higher-kinded data are records where all fields in the record
are all either of the type @Column f T@ (for any @T@), or are themselves
higher-kinded data:

[Nested]

@
data Nested f =
  Nested { nested1 :: MyType f
         , nested2 :: MyType f
         }
@

The @HigherKinded@ type class is used to give us a special mapping operation
that lets us change the type parameter @f@.

[Supplying @HigherKinded@ instances]

This type class should be derived generically for all table types in your
project. To do this, enable the @DeriveAnyType@ and @DeriveGeneric@ language
extensions:

@
\{\-\# LANGUAGE DeriveAnyClass, DeriveGeneric #-\}
import qualified GHC.Generics

data MyType f = MyType { fieldA :: Column f T }
  deriving ( GHC.Generics.Generic, HigherKinded )
@

-}
class HigherKinded ( t :: ( Type -> Type ) -> Type ) where
  -- | Additional constraints for use with 'zipRecord'.
  type ZipRecord t (f :: * -> *) (g :: * -> *) (c :: * -> Constraint) :: Constraint

  zipRecord
    :: forall c f g m proxy
     . ( ZipRecord t f g c, Applicative m )
    => proxy c
    -> (forall x. c x => C f x -> C f x -> m (C g x))
    -> t f -> t f -> m (t g)


mapRecord
  :: ( ZipRecord t f g Unconstrained, HigherKinded t )
  => ( forall x. C f x -> C g x ) -> t f -> t g
mapRecord f x =
  runIdentity ( zipRecord ( Proxy @Unconstrained ) ( \_ -> Identity . f ) x x )
