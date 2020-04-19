{-# language BlockArguments #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Null where

import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Indexed.Functor ( hmap )
import Data.Indexed.Functor.Compose ( HCompose(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..) )
import Data.Indexed.Functor.Traversable ( HTraversable(..) )
import Rel8.Column
import Rel8.Table


data Null a = Null (Maybe a)
  deriving (Show)


instance Table a => Table (Null a) where
  type Schema (Null a) =
    HCompose (Schema a) Maybe

  from (Null (Just x)) =
    HCompose $ hmap (\(Identity y) -> Compose $ Identity $ Just y) (from x)

  from (Null Nothing) =
    HCompose $ htabulate \_ -> Compose $ Identity Nothing

  to (HCompose x) =
    Null $ to <$> htraverse (\(Compose (Identity y)) -> Identity <$> y) x

  encode =
    HCompose $ hmap (Compose . nullEncoder) $ encode @a

  decode =
    HCompose $ hmap (Compose . maybeColumnDecoder) $ decode @a
