{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language RankNTypes #-}
{-# language TypeFamilyDependencies #-}

module Rel8.GigThinking where

import Data.Kind


data CONTEXT where
  HASKELL :: CONTEXT
  EXPR :: m -> CONTEXT
  TONULL :: CONTEXT -> CONTEXT


data NULL a = NULL a | NOTNULL a


type family Column ( context :: CONTEXT ) ( a :: NULL Type ) :: Type where
  Column ( 'EXPR m ) a = Expr m a
  Column 'HASKELL ( 'NULL a ) = Maybe a
  Column 'HASKELL ( 'NOTNULL a ) = a
  Column ( 'TONULL ( EXPR m ) ) ( _ a ) = Expr m ( 'NULL a )
  Column ( 'TONULL HASKELL ) ( _ a ) = Maybe a


type family Col ( context :: CONTEXT ) ( a :: NULL Type ) :: Type =
  Col f ( Maybe a ) = Column f ( 'NULL a )
  Col f a = Column f ( 'NOTNULL a )


newtype Expr ( m :: Type -> Type ) ( a :: NULL Type ) =
  Expr ()


newtype C ( context :: CONTEXT ) ( a :: NULL Type ) =
  C { toColumn :: Column context a }


class Table ( t :: Type ) where
  data Field t :: NULL Type -> Type

  type Context t :: CONTEXT

  index :: t -> Field t a -> C ( Context t ) a

  tabulate :: ( forall x. Field t x -> C ( Context t ) x ) -> t


instance Table ( Expr m ( f a ) ) where
  data Field ( Expr m ( f a ) ) x where
    ExprField :: Field ( Expr m ( f a ) ) ( f a )

  type Context ( Expr m ( f a ) ) =
    'EXPR m

  index x ExprField =
    C x

  tabulate f =
    toColumn ( f ExprField )


