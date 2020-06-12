{-# language ApplicativeDo #-}
{-# language ConstraintKinds #-}
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
         nullTag :: Expr Bool
       , table :: t
       }
    -> MaybeTable t


data MaybeTableField t a where
  MaybeTableIsNull :: MaybeTableField t Bool
  MaybeTableField :: Field t a -> MaybeTableField t a


instance (Table t, Context t ~ Expr) => Table (MaybeTable t) where
  type Field (MaybeTable t) = MaybeTableField t

  type Context (MaybeTable t) = Context t

  type ConstrainTable (MaybeTable t) c = ( c Bool, ConstrainTable t c )

  field MaybeTable{ nullTag, table } = \case
    MaybeTableIsNull ->
      MkC nullTag

    MaybeTableField i ->
      field table i

  tabulateMCP proxy f =
    MaybeTable
      <$> do toColumn <$> f MaybeTableIsNull
      <*> tabulateMCP proxy ( f . MaybeTableField )


maybeTable :: b -> (a -> b) -> MaybeTable a -> b
maybeTable def f MaybeTable{ nullTag, table } = f table
