{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}

module Rel8.HigherKinded where

import GHC.Exts ( Constraint )
import Rel8.Column ( C )


class HigherKinded t where
  type ZipRecord t (f :: * -> *) (g :: * -> *) (c :: * -> Constraint) :: Constraint

  zipRecord
    :: forall c f g m proxy
     . ( ZipRecord t f g c, Applicative m )
    => proxy c
    -> (forall x. c x => C f x -> C f x -> m (C g x))
    -> t f -> t f -> m (t g)
