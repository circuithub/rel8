{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Rewrite ( Rewrite(..) ) where

import Data.Functor.Identity
import Data.Proxy
import Rel8.Column
import Rel8.HigherKinded
import Rel8.Top


class Rewrite f g a b | f g a -> b, a -> f, b -> g, f g b -> a where
  rewrite :: ( forall x. C f x -> C g x ) -> a -> b


instance ( HigherKinded t, f ~ u, g ~ v, ZipRecord t u v Top ) => Rewrite f g ( t u ) ( t v ) where
  rewrite f t =
    runIdentity ( zipRecord (Proxy @Top) ( \_ -> pure . f ) t t )
