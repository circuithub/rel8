{-# language ApplicativeDo #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.MaybeTable ( HoldsUnderMaybe, MaybeTable(..), toMaybe ) where

import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import Rel8.Column
import Rel8.Context
import Rel8.Null
import Rel8.Table


{-| @MaybeTable t@ is the table @t@, but as the result of an outer join. If the
outer join fails to match any rows, this is essentialy @Nothing@, and if the
outer join does match rows, this is like @Just@.

-}
data MaybeTable t where
  MaybeTable
    :: ( Table t', TableContext t' ~ 'ToNull ( TableContext t ), Rewrite t 'ToNull, t' ~ Here t )
    => { -- | Check if this @MaybeTable@ is null. In other words, check if an outer
         -- join matched any rows.
         isNullTable :: MkColumn ( TableContext t ) ( 'NotNull Bool )
       , maybeTable :: t'
       } -> MaybeTable t


class ( Compatible t ( TableContext t ) ( Here t ) ( 'ToNull ( TableContext t ) ), Table t, Table ( Here t ), TableContext ( Here t ) ~ f ( TableContext t ) ) => Rewrite ( t :: Type ) ( f :: Context -> Context ) where
  type Here t :: Type


data MaybeTableField t ( a :: Null Type ) where
  MaybeTableIsNull :: MaybeTableField t ( NotNull Bool )
  MaybeTableField :: Field ( Here t ) ( null a ) -> MaybeTableField t ( 'Null a )


-- instance Compatible t f ~ g s => Compatible ( MaybeTable f t ) u ( MaybeTable g s ) v where
--   transferField MaybeTableIsNull = MaybeTableIsNull
--   transferField ( MaybeTableField f ) = MaybeTableField ( transferField f )


class c ( Maybe x ) => HoldsUnderMaybe c x


instance c ( Maybe x ) => HoldsUnderMaybe c x


class MkNull ( a :: Null Type ) where
  mkNull :: proxy a -> C ( 'ToNull f ) ( null x ) -> C f ( 'Null x )



instance ( ConstrainTable ( Here t ) Unconstrained, forall null. MkNull null, Table t, Rewrite t 'ToNull ) => Table ( MaybeTable t ) where
  type Field ( MaybeTable t ) =
    MaybeTableField t

  type ConstrainTable ( MaybeTable t ) c =
    ( c Bool, ConstrainTable t c, ConstrainTable ( Here t ) c )

  type TableContext ( MaybeTable t ) =
    TableContext t

  field MaybeTable{ isNullTable, maybeTable } = \case
    MaybeTableIsNull ->
      MkC isNullTable

    MaybeTableField i ->
      case field maybeTable i of
        MkC x -> MkC x

  tabulateMCP proxy f =
    MaybeTable
      <$> do toColumn <$> f MaybeTableIsNull
      <*> tabulateMCP @( Here t ) proxy ( \i -> MkC . toColumn <$> f ( MaybeTableField i ) )


-- | If you 'Rel8.Query.select' a @MaybeTable@, you'll get back a @MaybeTable@
-- as a result. However, this structure is awkward to use in ordinary Haskell,
-- as it's a normal record where all of the fields are wrapped in 'Nothing'.
-- 'toMaybe' lets you transform a @MaybeTable@ into a normal @Maybe@ value.
toMaybe
  :: ( Table notNull
     , TableContext null ~ 'Haskell
     )
  => MaybeTable null -> Maybe notNull
toMaybe MaybeTable{ isNullTable, maybeTable }
  | isNullTable = Nothing
  | otherwise = traverseTable sequenceC maybeTable
