{-# language TupleSections #-}

module Rel8.Query.Opaleye
  ( fromOpaleye
  , toOpaleye
  , mapOpaleye
  , zipOpaleyeWith
  )
where

-- base
import Control.Applicative ( liftA2 )
import Prelude

-- opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Query ( Query( Query ) )


fromOpaleye :: Opaleye.Select a -> Query a
fromOpaleye = Query . pure . fmap pure


toOpaleye :: Query a -> Opaleye.Select a
toOpaleye (Query a) = snd <$> a mempty


mapOpaleye :: (Opaleye.Select a -> Opaleye.Select b) -> Query a -> Query b
mapOpaleye f (Query a) = Query (fmap (mapping f) a)


zipOpaleyeWith :: ()
  => (Opaleye.Select a -> Opaleye.Select b -> Opaleye.Select c)
  -> Query a -> Query b -> Query c
zipOpaleyeWith f (Query a) (Query b) = Query $ liftA2 (zipping f) a b


mapping :: ()
  => (Opaleye.Select a -> Opaleye.Select b)
  -> Opaleye.Select (m, a) -> Opaleye.Select (m, b)
mapping f q@(Opaleye.QueryArr qa) = Opaleye.QueryArr $ \(_, query, tag) ->
  let
    ((m, _), _, _) = qa ((), query, tag)
    Opaleye.QueryArr qa' = (m,) <$> f (snd <$> q)
  in
    qa' ((), query, tag)


zipping :: Semigroup m
  => (Opaleye.Select a -> Opaleye.Select b -> Opaleye.Select c)
  -> Opaleye.Select (m, a) -> Opaleye.Select (m, b) -> Opaleye.Select (m, c)
zipping f q@(Opaleye.QueryArr qa) q'@(Opaleye.QueryArr qa') =
  Opaleye.QueryArr $ \(_, query, tag) ->
    let
      ((m, _), _, _) = qa ((), query, tag)
      ((m', _), _, _) = qa' ((), query, tag)
      m'' = m <> m'
      Opaleye.QueryArr qa'' = (m'',) <$> f (snd <$> q) (snd <$> q')
    in
      qa'' ((), query, tag)
