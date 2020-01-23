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


{-| @MaybeTable t@ is the table @t@, but as the result of an outer join. If the
outer join fails to match any rows, this is essentialy @Nothing@, and if the
outer join does match rows, this is like @Just@.

-}
data MaybeTable t =
  MaybeTable
    { isNullTable :: Column ( Context t ) Bool
    , maybeTable :: Rewrite t Null
    }


-- instance ( Table ( Rewrite t Null ), ConstrainTable ( Rewrite t Null ) Unconstrained, Table t, Rewritable t Null ) => Table ( MaybeTable t ) where
instance ( Rewritable t Null, Table ( Rewrite t Null ), Table t, ConstrainTable ( Rewrite t Null ) Unconstrained, ConstrainTable ( Rewrite t Null ) ( HoldsUnderMaybe Unconstrained ) ) => Table ( MaybeTable t ) where
  type Field ( MaybeTable t ) =
    MaybeTableField t

  type Context ( MaybeTable t ) =
    Context t

  type ConstrainTable ( MaybeTable t ) c =
    ( c Bool
    , ConstrainTable ( Rewrite t Null ) ( HoldsUnderMaybe c )
    , ConstrainTable ( Rewrite t Null ) c
    )

  field MaybeTable{ isNullTable, maybeTable } = \case
    IsNullField ->
      MkC isNullTable

    MaybeTableField i ->
      case field maybeTable i of
        MkC x -> MkC x

  tabulateMCP
    :: forall proxy c m
     . ( Applicative m, ConstrainTable ( MaybeTable t ) c )
    => proxy c
    -> ( forall x. c x => Field ( MaybeTable t ) x -> m ( C ( Context t ) x ) )
    -> m ( MaybeTable t )
  tabulateMCP _ f =
    MaybeTable <$> do toColumn <$> f IsNullField
               <*> tabulateMCP
                     ( Proxy @( HoldsUnderMaybe c ) )
                     ( \i -> ( \( MkC x ) -> MkC x ) <$> f ( MaybeTableField i ) )

data MaybeTableField t a where
  IsNullField :: MaybeTableField t Bool
  MaybeTableField :: Field ( Rewrite t Null ) a -> MaybeTableField t ( Maybe a )


instance ( Compatible ( Rewrite s Null ) ( Context ( Rewrite s Null ) ) ( Rewrite t Null ) ( Context ( Rewrite t Null ) ), Context s ~ u, Context t ~ v, Compatible s u t v ) => Compatible ( MaybeTable s ) u ( MaybeTable t ) v where
  transferField = \case
    IsNullField -> IsNullField
    MaybeTableField f -> MaybeTableField ( transferField f )


class c ( Maybe x ) => HoldsUnderMaybe c x


instance c ( Maybe x ) => HoldsUnderMaybe c x


-- | If you 'Rel8.Query.select' a @MaybeTable@, you'll get back a @MaybeTable@
-- as a result. However, this structure is awkward to use in ordinary Haskell,
-- as it's a normal record where all of the fields are wrapped in 'Nothing'.
-- 'toMaybe' lets you transform a @MaybeTable@ into a normal @Maybe@ value.
-- toMaybe
--   :: ( CompatibleTables null notNull
--      , Compatible notNull Identity null ( Null Identity )
--      )
--   => MaybeTable Identity null -> Maybe notNull
toMaybe
  :: forall t
   . ( Table t
     , Compatible t Identity ( Rewrite ( Rewrite t Null ) Selecting ) ( Null Identity )
     , Context ( Rewrite ( Rewrite t Null ) Selecting ) ~ Null Identity
     , Rewritable ( Rewrite t Null ) Selecting
     , Table ( Rewrite ( Rewrite t Null ) Selecting )
     , RewriteContext Selecting ( Null Identity )
     , Table ( Rewrite t Null )
     )
  => MaybeTable t -> Maybe t
toMaybe MaybeTable{ isNullTable, maybeTable }
  | isNullTable = Nothing
  | otherwise = traverseTable ( \( MkC x ) -> MkC <$> x ) ( rewrite @( Rewrite t Null ) @Selecting maybeTable )
