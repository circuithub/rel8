{-# language DeriveFunctor #-}
{-# language DerivingVia #-}
{-# language PolyKinds #-}
{-# language TypeFamilies #-}

-- | Poly-kinded Tagged, until https://github.com/ekmett/adjunctions/pull/68 is
-- merged.
module Data.Tagged.PolyKinded where

import Data.Coerce ( coerce )
import Data.Distributive ( Distributive(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Functor.Rep ( Representable(..), pureRep, apRep )


newtype Tagged (a :: k) x =
  Tagged { unTagged :: x }
  deriving (Functor, Foldable, Representable) via Identity
  deriving (Semigroup, Monoid) via Identity x


instance Distributive (Tagged a) where
  distribute y =
    Tagged $ fmap coerce y


instance Traversable (Tagged a) where
  traverse f (Tagged x) =
    Tagged <$> f x


instance Applicative (Tagged a) where
  pure =
    pureRep

  (<*>) =
    apRep
