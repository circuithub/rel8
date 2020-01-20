{-# language ApplicativeDo #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.MaybeTable ( MaybeTable(..), toMaybe ) where

import Data.Functor.Identity
import Rel8.Column
import Rel8.Table


{-| @MaybeTable t@ is the table @t@, but as the result of an outer join. If the
outer join fails to match any rows, this is essentialy @Nothing@, and if the
outer join does match rows, this is like @Just@.

-}
data MaybeTable f t =
  MaybeTable
    { -- | Check if this @MaybeTable@ is null. In other words, check if an outer
      -- join matched any rows.
      isNull :: Column f Bool
    , maybeTable :: t
    }


data MaybeTableField t ( f :: * -> * ) a where
  MaybeTableIsNull :: MaybeTableField t f Bool
  MaybeTableField :: Field t a -> MaybeTableField t f a


instance ( f ~ u, g ~ v, Compatible t f s g ) => Compatible ( MaybeTable f t ) u ( MaybeTable g s ) v where
  transferField MaybeTableIsNull = MaybeTableIsNull
  transferField ( MaybeTableField f ) = MaybeTableField ( transferField f )


instance ( Context t ~ f, Table t ) => Table ( MaybeTable f t ) where
  type Field ( MaybeTable f t ) =
    MaybeTableField t f

  type ConstrainTable ( MaybeTable f t ) c =
    ( c Bool, ConstrainTable t c )

  type Context ( MaybeTable f t ) =
    f

  field MaybeTable{ isNull, maybeTable } = \case
    MaybeTableIsNull -> C isNull
    MaybeTableField f -> field maybeTable f

  tabulateMCP proxy f =
    MaybeTable
      <$> do toColumn <$> f MaybeTableIsNull
      <*> tabulateMCP proxy ( f . MaybeTableField )


-- | If you 'Rel8.Query.select' a @MaybeTable@, you'll get back a @MaybeTable@
-- as a result. However, this structure is awkward to use in ordinary Haskell,
-- so 'toMaybe' lets you transform a @MaybeTable@ into a normal @Maybe@ value.
toMaybe :: MaybeTable Identity t -> Maybe t
toMaybe MaybeTable{ isNull, maybeTable }
  | isNull = Nothing
  | otherwise = Just maybeTable
