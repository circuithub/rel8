{-# language TupleSections #-}

module Rel8.Query.Opaleye
  ( fromOpaleye
  , toOpaleye
  , mapOpaleye
  , zipOpaleyeWith
  , unsafePeekQuery
  )
where

-- base
import Control.Applicative ( liftA2 )
import Prelude

-- opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye

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


unsafePeekQuery :: Query a -> a
unsafePeekQuery (Query q) = case q mempty of
  Opaleye.QueryArr f -> case f ((), Opaleye.start) of
    ((_, a), _, _) -> a


mapping :: ()
  => (Opaleye.Select a -> Opaleye.Select b)
  -> Opaleye.Select (m, a) -> Opaleye.Select (m, b)
mapping f q@(Opaleye.QueryArr qa) = Opaleye.QueryArr $ \(_, tag) ->
  let
    ((m, _), _, _) = qa ((), tag)
    Opaleye.QueryArr qa' = (m,) <$> f (snd <$> q)
  in
    qa' ((), tag)


zipping :: Semigroup m
  => (Opaleye.Select a -> Opaleye.Select b -> Opaleye.Select c)
  -> Opaleye.Select (m, a) -> Opaleye.Select (m, b) -> Opaleye.Select (m, c)
zipping f q@(Opaleye.QueryArr qa) q'@(Opaleye.QueryArr qa') =
  Opaleye.QueryArr $ \(_, tag) ->
    let
      ((m, _), _, _) = qa ((), tag)
      ((m', _), _, _) = qa' ((), tag)
      m'' = m <> m'
      Opaleye.QueryArr qa'' = (m'',) <$> f (snd <$> q) (snd <$> q')
    in
      qa'' ((), tag)
