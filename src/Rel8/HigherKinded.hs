{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.HigherKinded where

import Data.Functor.Identity
import Data.Proxy
import GHC.Exts ( Constraint )
import Rel8.Column ( C )
import Rel8.Top ( Top )


class HigherKinded t where
  type ZipRecord t (f :: * -> *) (g :: * -> *) (c :: * -> Constraint) :: Constraint

  zipRecord
    :: forall c f g m proxy
     . ( ZipRecord t f g c, Applicative m )
    => proxy c
    -> (forall x. c x => C f x -> C f x -> m (C g x))
    -> t f -> t f -> m (t g)


mapRecord
  :: ( ZipRecord t f g Top, HigherKinded t )
  => ( forall x. C f x -> C g x ) -> t f -> t g
mapRecord f x =
  runIdentity ( zipRecord ( Proxy @Top ) ( \_ -> Identity . f ) x x )
