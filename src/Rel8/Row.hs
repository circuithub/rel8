{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Row where

import Data.Coerce ( coerce )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Contravariant ( Op(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.Indexed.Functor ( hmap )
import Data.Indexed.Functor.Compose ( HCompose(..) )
import Data.Indexed.Functor.Identity ( HIdentity(..) )
import Data.Indexed.Functor.Product ( HProduct(..) )
import Data.Indexed.Functor.Representable ( HRepresentable(..), hzipWith )
import Data.Indexed.Functor.Traversable ( htraverse, hsequence )
import Data.String ( IsString(..) )
import Data.Tagged.PolyKinded ( Tagged(..) )
import GHC.Records.Compat ( HasField(..) )
import GHC.TypeLits ( Symbol )
import Rel8.Column ( Column )
import qualified Rel8.Column as Column
import Rel8.Table ( Table(..) )


-- | Typed expressions.
newtype Row a =
  Row (Pattern a Column)


traverseColumns
  :: (Applicative f, Table a)
  => (forall x. Column x -> f (Column x))
  -> Row a -> f (Row a)
traverseColumns f (Row x) = fmap Row $ htraverse f x


zipColumnsM
  :: (Applicative m, Table a)
  => (forall x. Column x -> Column x -> m (Column x))
  -> Row a -> Row a -> m (Row a)
zipColumnsM f (Row x) (Row y) =
  fmap Row $ hsequence $ htabulate \i -> Compose $ f (hindex x i) (hindex y i)


sequenceColumns :: (Table a, Applicative m) => (forall x. m (Column x)) -> m (Row a)
sequenceColumns m = fmap Row $ hsequence $ htabulate \_ -> Compose m


toColumns :: Row a -> Pattern a Column
toColumns (Row x) = x


instance (HasField name a r, HasField name (Pattern a Column) (Pattern r Column)) => HasField (name :: Symbol) (Row a) (Row r) where
  hasField (Row x) = (setter, getter) where
    setter (Row r) = Row $ fst (hasField @name x) r
    getter = Row $ snd $ hasField @name x


fst_ :: Row (a, b) -> Row a
fst_ (Row (Compose (Tagged (HProduct x _)))) = Row x


snd_ :: Row (a, b) -> Row b
snd_ (Row (Compose (Tagged (HProduct _ y)))) = Row y


isNothing :: Table a => Row (Maybe a) -> Row Bool
isNothing = maybe_ (lit True) (const $ lit False)


maybe_ :: (Table a, Table b) => Row b -> (Row a -> Row b) -> Row (Maybe a) -> Row b
maybe_ (Row def) f (Row (Compose (Tagged (HProduct (HIdentity isNull) (HCompose row))))) = Row $ htabulate \i ->
  Column.case_
    [(isNull, hindex def i)]
    (hindex (toColumns (f (Row $ hmap (coerce Column.fromJust) row))) i)


lit :: forall a. Table a => a -> Row a
lit = Row . hzipWith (\f x -> Column.lit (coerce f x)) (encode @a) . from


instance (Table a, IsString a) => IsString (Row a) where
  fromString = lit . fromString
