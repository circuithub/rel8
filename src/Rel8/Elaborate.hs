{-# language MultiParamTypeClasses #-}
{-# language BlockArguments #-}
{-# language TypeApplications #-}
{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language TypeFamilyDependencies #-}

module Rel8.Elaborate where

import Data.Proxy
import Data.Functor.Identity
import Data.Kind

class Table t where
  data Field t :: ( Type -> Type ) -> Type -> Type

  index :: t -> Field t f x -> C f x

  type ConstrainTable t ( c :: Type -> Constraint ) :: Constraint

  tabulate :: ( Applicative m, ConstrainTable t c ) => proxy c -> ( forall f x. c x => Field t f x -> m ( C f x ) ) -> m t

type family Column ( f :: Type -> Type ) ( a :: Type ) :: Type where
  Column ( Null ( Expr m ) ) a = NullExpr m a
  Column ( Null ( NullExpr m ) ) a = NullExpr m a
  Column Identity a = a
  Column f a = f a

newtype C f a = C { unC :: Column f a }

data Null ( f :: Type -> Type ) a

data Expr ( m :: * -> * ) ( a :: Type ) =
  Expr ()

data NullExpr ( m :: * -> * ) ( a :: Type ) =
  NullExpr ()

instance Table ( Expr m a ) where
  data Field ( Expr m a ) f x where
    ExprField :: Field ( Expr m a ) ( Expr m ) a

  type ConstrainTable ( Expr m a ) c =
    c a

  index x ExprField =
    C x

  tabulate _ f =
    unC <$> f ExprField

instance Table ( NullExpr m a ) where
  data Field ( NullExpr m a ) f x where
    NullExprField :: Field ( NullExpr m a ) ( Null ( Expr m ) ) a

  index x NullExprField =
    C x

  type ConstrainTable ( NullExpr m a ) c =
    c a

  tabulate proxy f =
    unC <$> f NullExprField

data TheThingIWant f =
  TheThingIWant { grr :: Column ( Null f ) Bool }

instance Table ( TheThingIWant ( Expr m ) ) where
  data Field ( TheThingIWant ( Expr m ) ) f a where
    Grr :: Field ( TheThingIWant ( Expr m ) ) ( NullExpr m ) Bool

  index TheThingIWant{ grr } Grr = C grr

  type ConstrainTable ( TheThingIWant ( Expr m ) ) c = c Bool

  tabulate _ f = TheThingIWant . unC <$> f Grr

instance Table ( TheThingIWant ( NullExpr m ) ) where
  data Field ( TheThingIWant ( NullExpr m ) ) f a where
    GrrNull :: Field ( TheThingIWant ( NullExpr m ) ) ( NullExpr m ) Bool

  index TheThingIWant{ grr } GrrNull =
    C grr

  type ConstrainTable ( TheThingIWant ( NullExpr m ) ) c = c Bool

  tabulate _ f = TheThingIWant . unC <$> f GrrNull

instance Compatible ( TheThingIWant ( NullExpr m ) ) ( TheThingIWant ( Expr m ) )

theThingIWant1ToTheThingIWant2
  :: TheThingIWant ( Expr m )
  -> TheThingIWant ( NullExpr m )
theThingIWant1ToTheThingIWant2 x =
  runIdentity $ traverseTable ( \( C x ) -> Identity ( C _ ) ) x


class ( Table x, Table y ) => Compatible x y where
  transferField :: Field x f a -> Field y g a

traverseTable
  :: forall t' t m
   . ( Applicative m, Table t, Table t', ConstrainTable t' Unconstrained, Compatible t' t )
  => ( forall f g x. C f x -> m ( C g x ) )
  -> t
  -> m t'
traverseTable f t =
  tabulate ( Proxy @Unconstrained ) \i ->
    f ( index t ( transferField i ) )

class Unconstrained a
instance Unconstrained a
