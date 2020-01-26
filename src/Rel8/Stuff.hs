{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Stuff where

import Data.Kind
import GHC.TypeLits

type family NotMaybe ( a :: Type ) :: Constraint where
  NotMaybe ( Maybe a ) = TypeError ( 'Text "!" )
  NotMaybe a = ()
