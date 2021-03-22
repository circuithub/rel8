{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.HTable.HMapTable ( HMapTable(..), Exp, Eval, MapInfo(..), Precompose(..), HMapTableField(..) ) where

-- base
import Data.Kind ( Constraint, Type )

-- rel8
import Rel8.Context ( Column, Meta )
import Rel8.HTable ( Column( DictColumn ), Dict, HAllColumns, HField, HTable, hdbtype, hdict, hfield, htabulate, htabulateMeta, htraverse )
import Rel8.Info ( Info )

-- semigroupoids
import Data.Functor.Apply ( Apply )


type Exp :: Type -> Type


type Exp a = a -> Type


type family Eval (e :: Exp a) :: a


data HMapTable :: (Meta -> Exp Meta) -> ((Meta -> Type) -> Type) -> (Meta -> Type) -> Type where
  HMapTable :: { unHMapTable :: t (Precompose f g) } -> HMapTable f t g


newtype Precompose :: (Meta -> Exp Meta) -> (Meta -> Type) -> Meta -> Type where
  Precompose :: { precomposed :: g (Eval (f x)) } -> Precompose f g x


data HMapTableField :: (Meta -> Exp Meta) -> ((Meta -> Type) -> Type) -> Meta -> Type where
  HMapTableField :: HField t a -> HMapTableField f t (Eval (f a))


instance (HTable t, MapInfo f) => HTable (HMapTable f t) where
  type HField (HMapTable f t) = HMapTableField f t

  type HAllColumns (HMapTable f t) c =
    HAllColumns t (ComposeConstraint f c)

  hfield (HMapTable x) (HMapTableField i) =
    case hfield x i of
      Precompose y -> y

  htabulate f = HMapTable $ htabulate (Precompose . f . HMapTableField)

  htraverse :: forall g h m. Apply m
    => (forall x. g x -> m (h x)) -> HMapTable f t g -> m (HMapTable f t h)
  htraverse f (HMapTable x) = HMapTable <$> htraverse go x
    where
      go :: forall x. Precompose f g x -> m (Precompose f h x)
      go (Precompose a) = Precompose <$> f a

  hdict :: forall c. HAllColumns (HMapTable f t) c => HMapTable f t (Column (Dict c))
  hdict = htabulateMeta \(HMapTableField j) ->
    case hfield (hdict @_ @(ComposeConstraint f c)) j of
      DictColumn -> DictColumn

  hdbtype = HMapTable $ htabulate \i ->
    case hfield hdbtype i of
      x -> Precompose (mapInfo @f x)


class MapInfo f where
  mapInfo :: Column Info x -> Column Info (Eval (f x))


class c (Eval (f a)) => ComposeConstraint (f :: Meta -> Exp Meta) (c :: Meta -> Constraint) (a :: Meta)
