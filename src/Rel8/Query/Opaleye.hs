module Rel8.Query.Opaleye
  ( fromOpaleye
  , toOpaleye
  , mapOpaleye
  , zipOpaleyeWith
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Data.Monoid ( Endo( Endo ) )
import Prelude

-- opaleye
import qualified Opaleye.Select as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Query ( Query( Query ) )

-- transformers
import Control.Monad.Trans.State.Strict ( runState )


fromOpaleye :: Opaleye.Select a -> Query a
fromOpaleye a = Query (pure (mempty, a))


toOpaleye :: Query a -> Opaleye.Select a
toOpaleye (Query s) = Opaleye.QueryArr $ \(_, query, tag) ->
  let
    ((Endo modify, qff), tag') = runState s tag
    Opaleye.QueryArr qf = qff
    lquery = modify query
    (a, rquery, tag'') = qf ((), Opaleye.Unit, tag')
    query' =
      Opaleye.Product
        ((Opaleye.NonLateral, lquery) :| [(Opaleye.Lateral, rquery)])
        []
  in
    (a, query', tag'')


mapOpaleye :: (Opaleye.Select a -> Opaleye.Select b) -> Query a -> Query b
mapOpaleye f (Query s) = Query ((fmap . fmap) f s)


zipOpaleyeWith :: ()
  => (Opaleye.Select a -> Opaleye.Select b -> Opaleye.Select c)
  -> Query a -> Query b -> Query c
zipOpaleyeWith f (Query s) (Query s') = Query $
  liftA2 (\(modify, x) (modify', x') -> (modify <> modify', f x x')) s s'
