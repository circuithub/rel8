{-# language ConstraintKinds #-}
{-# language EmptyDataDecls #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Recontextualise
  ( Apply
  , Lit
  , Recontextualise (MapContext, mapContext)
  )
where

import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Type )
import Rel8.Column ( C( MkC ) )
import Rel8.Expr ( Expr )
import Rel8.Table


type family Apply (g :: (Type -> Type) -> Type -> Type) (f :: Type -> Type) :: Type -> Type where
  Apply Lit Identity = Expr
  Apply g f = g f


data Lit (f :: Type -> Type) a


class
  ( Table a
  , Table b
  , MapContext g a ~ b
  , Structure a ~ Structure b
  , Apply g (Context a) ~ Context b
  ) =>
    Recontextualise g a b
  where

  type MapContext (g :: (Type -> Type) -> Type -> Type) a :: Type

  mapContext :: (Applicative m, HConstrainTable (Structure a) (Context b) c)
    => proxy g
    -> proxy' c
    -> (forall x. c x => C (Context a) x -> m (C (Context b) x))
    -> a
    -> m b


instance (HigherKindedTable t, f' ~ Apply g f) => Recontextualise g (t f) (t f') where
  type MapContext g (t f) = t (Apply g f)

  mapContext _ c f as = htabulate c (\field -> f (hfield as field))


instance (Recontextualise g a a', Recontextualise g b b', Context a ~ Context b) => Recontextualise g (a, b) (a', b') where
  type MapContext g (a, b) = (MapContext g a, MapContext g b)

  mapContext g c f (a, b) = (,) <$> mapContext g c f a <*> mapContext g c f b


-- | FIXME: This isn't quite right
instance Recontextualise Lit (Identity a) (Expr a) where
  type MapContext Lit (Identity a) = Expr a

  mapContext _ _ f (Identity a) = unC <$> f (MkC a)
    where
      unC (MkC x) = x
