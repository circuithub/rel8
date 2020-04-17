{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
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
  Row (Schema a Column)


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


toColumns :: Row a -> Schema a Column
toColumns (Row x) = x


instance (HasField name a r, HasField name (Schema a Column) (Schema r Column)) => HasField (name :: Symbol) (Row a) (Row r) where
  hasField (Row x) = (setter, getter) where
    setter (Row r) = Row $ fst (hasField @name x) r
    getter = Row $ snd $ hasField @name x


isNothing :: Table a => Row (Maybe a) -> Row Bool
isNothing = maybe_ (lit True) (const $ lit False)


maybe_ :: (Table a, Table b) => Row b -> (Row a -> Row b) -> Row (Maybe a) -> Row b
maybe_ (Row def) f (Row (Compose (Tagged (HProduct (HIdentity isNull) (HCompose row))))) = Row $ htabulate \i ->
  Column.case_
    [(isNull, hindex def i)]
    (hindex (toColumns (f (Row $ hmap (coerce Column.fromJust) row))) i)


lit :: forall a. Table a => a -> Row a
lit = Row . hzipWith (\f x -> Column.Column (coerce f x)) (encode @a) . from


instance (Table a, IsString a) => IsString (Row a) where
  fromString = lit . fromString


class Table a => EqTable a where
  (==.) :: Row a -> Row a -> Row Bool


class Table y => RowProduct x y | x -> y, y -> x where
  toRow :: x -> Row y
  fromRow :: Row y -> x


instance (Table x', Table y', x ~ Row x', y ~ Row y') => RowProduct (x, y) (x', y') where
  toRow (Row a, Row b) = Row $ Compose $ Tagged $ HProduct a b
  fromRow (Row (Compose (Tagged (HProduct l r)))) = (Row l, Row r)


data MaybeRow a = MaybeRow { rowIsNull :: Row Bool, row :: a }
  deriving (Functor)


instance (a ~ Row b, Table b) => RowProduct (MaybeRow a) (Maybe b) where
  toRow MaybeRow{ rowIsNull, row } =
    Row $ Compose $ Tagged $ HProduct (toColumns rowIsNull) (HCompose $ hmap coerce $ toColumns row)

  fromRow (Row (Compose (Tagged (HProduct a (HCompose b))))) =
    MaybeRow (Row a) (Row $ hmap coerce b)


underRowProduct :: (RowProduct t b, RowProduct s a) => (s -> t) -> Row a -> Row b
underRowProduct f = toRow . f . fromRow
