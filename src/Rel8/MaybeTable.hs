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
       , maybeTable :: t
       }
    -> MaybeTable t


-- data MaybeTableField t a where
--   MaybeTableIsNull :: MaybeTableField t ( Maybe Bool )
--   MaybeTableField :: Field ( MapTable Null t ) a -> MaybeTableField t ( Maybe ( DropMaybe a ) )


-- class c ( Maybe ( DropMaybe x ) ) => HoldsUnderMaybe c x


-- instance c ( Maybe ( DropMaybe x ) ) => HoldsUnderMaybe c x


instance Table t => Table (MaybeTable t) where
  type Context (MaybeTable t) = Context t

  type ConstrainTable (MaybeTable t) c = ConstrainTable t c
-- instance
--   ( Table ( MapTable Null t )
--   , ConstrainTable ( MapTable Null t ) Unconstrained
--   , ConstrainTable ( MapTable Null t ) ( HoldsUnderMaybe Unconstrained )
--   , Context ( MapTable Null t ) ~ Null ( Context t )
--   ) => Table ( MaybeTable t ) where
--   type Field ( MaybeTable t ) =
--     MaybeTableField t

--   type ConstrainTable ( MaybeTable t ) c =
--     ( c ( Maybe Bool )
--     , ConstrainTable ( MapTable Null t ) ( HoldsUnderMaybe c )
--     )

--   type Context ( MaybeTable t ) =
--     Context t

--   field MaybeTable{ nullTag, maybeTable } = \case
--     MaybeTableIsNull ->
--       MkC nullTag

--     MaybeTableField i ->
--       case field maybeTable i of
--         MkC x -> MkC x

--   tabulateMCP proxy f =
--     MaybeTable
--       <$> do toColumn <$> f MaybeTableIsNull
--       <*> tabulateMCP
--             ( holdsUnderMaybe proxy )
--             ( fmap ( \( MkC x ) -> MkC x ) . f . MaybeTableField )

--     where

--       holdsUnderMaybe :: proxy c -> Proxy ( HoldsUnderMaybe c )
--       holdsUnderMaybe _ = Proxy
