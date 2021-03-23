module Rel8.Query.Opaleye
  ( fromOpaleye
  , toOpaleye
  , mapOpaleye
  , zipOpaleyeWith
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Select as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Query ( Query( Query ) )


fromOpaleye :: Opaleye.Select a -> Query a
fromOpaleye = Query


toOpaleye :: Query a -> Opaleye.Select a
toOpaleye (Query a) = a


mapOpaleye :: (Opaleye.Select a -> Opaleye.Select b) -> Query a -> Query b
mapOpaleye f = fromOpaleye . f . toOpaleye


zipOpaleyeWith :: ()
  => (Opaleye.Select a -> Opaleye.Select b -> Opaleye.Select c)
  -> Query a -> Query b -> Query c
zipOpaleyeWith f a b = fromOpaleye $ f (toOpaleye a) (toOpaleye b)
