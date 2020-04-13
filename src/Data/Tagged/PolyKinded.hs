{-# language DeriveFunctor #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}

-- | Poly-kinded Tagged, until https://github.com/ekmett/adjunctions/pull/68 is
-- merged.
module Data.Tagged.PolyKinded where

import Data.Coerce ( coerce )
import Data.Distributive ( Distributive(..) )
import Data.Functor.Rep ( Representable(..), pureRep, apRep )


newtype Tagged (a :: k) x =
  Tagged { unTagged :: x }
  deriving (Functor)


instance Distributive (Tagged a) where
  distribute y = Tagged $ fmap coerce y


instance Representable (Tagged a) where
  type Rep (Tagged a) = ()
  index (Tagged x) () = x
  tabulate f = Tagged (f ())


instance Foldable (Tagged a) where
  foldMap f (Tagged a) = f a


instance Traversable (Tagged a) where
  traverse f (Tagged x) = Tagged <$> f x


instance Applicative (Tagged a) where
  pure = pureRep
  (<*>) = apRep
