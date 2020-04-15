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

module Rel8.Expr where

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
newtype Expr a =
  Expr (Pattern a Column)


traverseColumns
  :: (Applicative f, Table a)
  => (forall x. Column x -> f (Column x))
  -> Expr a -> f (Expr a)
traverseColumns f (Expr x) = fmap Expr $ htraverse f x


zipColumnsM
  :: (Applicative m, Table a)
  => (forall x. Column x -> Column x -> m (Column x))
  -> Expr a -> Expr a -> m (Expr a)
zipColumnsM f (Expr x) (Expr y) =
  fmap Expr $ hsequence $ htabulate \i -> Compose $ f (hindex x i) (hindex y i)


sequenceColumns :: (Table a, Applicative m) => (forall x. m (Column x)) -> m (Expr a)
sequenceColumns m = fmap Expr $ hsequence $ htabulate \_ -> Compose m


toColumns :: Expr a -> Pattern a Column
toColumns (Expr x) = x


instance (HasField name a r, HasField name (Pattern a Column) (Pattern r Column)) => HasField (name :: Symbol) (Expr a) (Expr r) where
  hasField (Expr x) = (setter, getter) where
    setter (Expr r) = Expr $ fst (hasField @name x) r
    getter = Expr $ snd $ hasField @name x


fst_ :: Expr (a, b) -> Expr a
fst_ (Expr (Compose (Tagged (HProduct x _)))) = Expr x


snd_ :: Expr (a, b) -> Expr b
snd_ (Expr (Compose (Tagged (HProduct _ y)))) = Expr y


isNothing :: Table a => Expr (Maybe a) -> Expr Bool
isNothing = maybe_ (lit True) (const $ lit False)


maybe_ :: (Table a, Table b) => Expr b -> (Expr a -> Expr b) -> Expr (Maybe a) -> Expr b
maybe_ (Expr def) f (Expr (Compose (Tagged (HProduct (HIdentity isNull) (HCompose row))))) = Expr $ htabulate \i ->
  Column.case_
    [(isNull, hindex def i)]
    (hindex (toColumns (f (Expr $ hmap (coerce Column.fromJust) row))) i)


lit :: forall a. Table a => a -> Expr a
lit = Expr . hzipWith (\f x -> Column.lit (coerce f x)) (encode @a) . from


instance (Table a, IsString a) => IsString (Expr a) where
  fromString = lit . fromString
