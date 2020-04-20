{-# language DerivingVia #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}

-- | Poly-kinded Tagged, until https://github.com/ekmett/adjunctions/pull/68 is
-- merged.

module Data.Tagged.PolyKinded ( Tagged( Tagged ), unTagged ) where

-- adjunctions
import Data.Functor.Rep ( Representable, apRep, pureRep )

-- base
import Data.Coerce ( coerce )
import Data.Functor.Compose ( Compose( Compose ) )
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Type )

-- distributive
import Data.Distributive ( Distributive, distribute )

-- record-hasfield
import GHC.Records.Compat ( HasField, hasField )


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


instance HasField name (g i) r => HasField name (Compose (Tagged (x :: Type)) g i) r where
  hasField (Compose (Tagged x)) = (setter, getter) where
    setter = Compose . Tagged . fst (hasField @name x)
    getter = snd $ hasField @name x
