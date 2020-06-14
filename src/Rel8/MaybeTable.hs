{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DeriveFunctor #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.MaybeTable where

import Data.Functor.Identity
import Data.Proxy
import Rel8.Column
import Rel8.Expr
import Rel8.Table


{-| @MaybeTable t@ is the table @t@, but as the result of an outer join. If the
outer join fails to match any rows, this is essentialy @Nothing@, and if the
outer join does match rows, this is like @Just@. Unfortunately, SQL makes it
impossible to distinguish whether or not an outer join matched any rows based
generally on the row contents - if you were to join a row entirely of nulls,
you can't distinguish if you matched an all null row, or if the match failed.
For this reason @MaybeTable@ contains an extra field - 'nullTag' - to
track whether or not the outer join produced any rows.

-}
data MaybeTable t where
  MaybeTable
    :: { -- | Check if this @MaybeTable@ is null. In other words, check if an outer
         -- join matched any rows.
         nullTag :: Expr ( Maybe Bool )
       , table :: t
       }
    -> MaybeTable t
  deriving
    ( Functor )


instance Applicative MaybeTable where
  pure = MaybeTable (lit (Just True))
  MaybeTable t f <*> MaybeTable t' a = MaybeTable (liftNull (or_ t t')) (f a)
    where
      or_ x y =
        null_ (lit False) (\x' -> null_ (lit False) (\y' -> x' ||. y') y) x


instance Monad MaybeTable where
  MaybeTable t a >>= f = case f a of
    MaybeTable t' b -> MaybeTable (liftNull (or_ t t')) b
    where
      or_ x y =
        null_ (lit False) (\x' -> null_ (lit False) (\y' -> x' ||. y') y) x



data MaybeTableField t a where
  MaybeTableIsNull :: MaybeTableField t ( Maybe Bool )
  MaybeTableField :: Field t a -> MaybeTableField t a


instance (Table t, Context t ~ Expr) => Table (MaybeTable t) where
  type Field (MaybeTable t) = MaybeTableField t

  type Context (MaybeTable t) = Context t

  type ConstrainTable (MaybeTable t) c = ( c ( Maybe Bool ), ConstrainTable t c )

  field MaybeTable{ nullTag, table } = \case
    MaybeTableIsNull ->
      MkC nullTag

    MaybeTableField i ->
      field table i

  tabulateMCP proxy f =
    MaybeTable
      <$> do toColumn <$> f MaybeTableIsNull
      <*> tabulateMCP proxy ( f . MaybeTableField )


maybeTable
  :: (Context b ~ Context a, Context a ~ Expr, Table a, Table b)
  => b -> (a -> b) -> MaybeTable a -> b
maybeTable def f MaybeTable{ nullTag, table } =
  ifThenElse_ (null_ (lit False) id nullTag) (f table) def


noTable :: (Table a, Context a ~ Expr) => MaybeTable a
noTable = MaybeTable tag t
  where
    tag = lit Nothing
    t = runIdentity $ tabulateMCP (Proxy @Unconstrained) f
      where
        f _ = pure $ MkC $ unsafeCoerceExpr $ lit (Nothing @Bool)
