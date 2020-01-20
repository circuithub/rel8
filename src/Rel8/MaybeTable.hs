{-# language ApplicativeDo #-}
{-# language ConstraintKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language InstanceSigs #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.MaybeTable ( HoldsUnderMaybe, MaybeTable(..), toMaybe ) where

import Data.Functor.Identity
import Data.Proxy
import Rel8.Column
import Rel8.Table
import Rel8.Unconstrained


{-| @MaybeTable t@ is the table @t@, but as the result of an outer join. If the
outer join fails to match any rows, this is essentialy @Nothing@, and if the
outer join does match rows, this is like @Just@.

-}
data MaybeTable f t where
  MaybeTable
    :: Context t ~ Null f
    => { -- | Check if this @MaybeTable@ is null. In other words, check if an outer
         -- join matched any rows.
         isNull :: Column f Bool
       , maybeTable :: t
       }
    -> MaybeTable f t


data MaybeTableField ( f :: * -> * ) t a where
  MaybeTableIsNull :: MaybeTableField f t Bool
  MaybeTableField :: Field t a -> MaybeTableField f t ( Maybe a )


instance ( f ~ u, g ~ v, Compatible t ( Null f ) s ( Null g ) ) => Compatible ( MaybeTable f t ) u ( MaybeTable g s ) v where
  transferField MaybeTableIsNull = MaybeTableIsNull
  transferField ( MaybeTableField f ) = MaybeTableField ( transferField f )


class c ( Maybe x ) => HoldsUnderMaybe c x


instance c ( Maybe x ) => HoldsUnderMaybe c x


instance ( ConstrainTable t ( HoldsUnderMaybe Unconstrained ), Context t ~ Null f, Table t ) => Table ( MaybeTable f t ) where
  type Field ( MaybeTable f t ) =
    MaybeTableField f t

  type ConstrainTable ( MaybeTable f t ) c =
    ( c Bool, ConstrainTable t ( HoldsUnderMaybe c ) )

  type Context ( MaybeTable f t ) =
    f

  field MaybeTable{ isNull, maybeTable } = \case
    MaybeTableIsNull ->
      C isNull

    MaybeTableField i ->
      case field maybeTable i of C x -> C x

  tabulateMCP
    :: forall proxy c m
     . ( Applicative m, ConstrainTable ( MaybeTable f t ) c )
    => proxy c
    -> ( forall x. c x => Field ( MaybeTable f t ) x -> m ( C f x ) )
    -> m ( MaybeTable f t )
  tabulateMCP _ f =
    MaybeTable
      <$> do toColumn <$> f MaybeTableIsNull
      <*> tabulateMCP
            ( Proxy @( HoldsUnderMaybe c ) )
            ( fmap ( \( C x ) -> C x ) . f . MaybeTableField )


-- | If you 'Rel8.Query.select' a @MaybeTable@, you'll get back a @MaybeTable@
-- as a result. However, this structure is awkward to use in ordinary Haskell,
-- as it's a normal record where all of the fields are wrapped in 'Nothing'.
-- 'toMaybe' lets you transform a @MaybeTable@ into a normal @Maybe@ value.
toMaybe
  :: ( CompatibleTables null notNull
     , Compatible notNull Identity null ( Null Identity )
     )
  => MaybeTable Identity null -> Maybe notNull
toMaybe MaybeTable{ isNull, maybeTable }
  | isNull = Nothing
  | otherwise = traverseTable ( \( C x ) -> C <$> x ) maybeTable
