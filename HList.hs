{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module HList where

import Control.Applicative ( Const(..) )

data HList f xs where
  HNil :: HList f '[]
  HCons :: f x -> HList f xs -> HList f ( x ': xs )


hmap :: ( forall x. f x -> g x ) -> HList f xs -> HList g xs
hmap f = \case
  HNil ->
    HNil

  HCons x xs ->
    HCons ( f x ) ( hmap f xs )


collect :: ( forall x. f x -> a ) -> HList f xs -> [ a ]
collect f = \case
  HNil ->
    []

  HCons x xs ->
    f x : collect f xs


type family xs ++ ys where
  '[] ++ ys = ys
  ( x ': xs ) ++ ys = x ': ( xs ++ ys )


happend :: HList f xs -> HList f ys -> HList f ( xs ++ ys )
happend HNil ys = ys
happend ( HCons x xs ) ys = HCons x ( happend xs ys )


class SList xs where
  slist :: HList ( Const () ) xs


instance SList '[] where
  slist = HNil


instance SList xs => SList ( x ': xs ) where
  slist =
    HCons ( Const () ) slist


split
  :: ( SList ys, xs ~ ( ys ++ zs ) )
  => HList f xs -> ( HList f ys, HList f zs )
split xs =
  go xs slist

  where

    go
      :: ( xs ~ ( ys ++ zs ) )
      => HList f xs
      -> HList g ys
      -> ( HList f ys, HList f zs )
    go zs HNil =
      ( HNil, zs )
