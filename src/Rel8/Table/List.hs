{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table.List
  ( ListTable(..)
  )
where

-- base
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Expr.Array ( sappend, sempty )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable.List ( HListTable )
import Rel8.Schema.HTable.Vectorize ( happend, hempty, hvectorize )
import Rel8.Schema.Nullability ( Nullability( Nullable, NonNullable ) )
import Rel8.Schema.Spec ( SSpec( SSpec ) )
import Rel8.Schema.Spec.ConstrainDBType ( dbTypeDict, dbTypeNullability )
import Rel8.Table ( Table, Context, Columns, fromColumns, toColumns )
import Rel8.Table.Alternative
  ( AltTable, (<|>:)
  , AlternativeTable, emptyTable
  )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Recontextualize ( Recontextualize )


-- | A @ListTable@ value contains zero or more instances of @a@. You construct
-- @ListTable@s with 'many' or 'listAgg'.
type ListTable :: Type -> Type
newtype ListTable a = ListTable (HListTable (Columns a) (Col (Context a)))


instance Table context a => Table context (ListTable a) where
  type Columns (ListTable a) = HListTable (Columns a)
  type Context (ListTable a) = Context a

  fromColumns = ListTable
  toColumns (ListTable a) = a


instance Recontextualize from to a b =>
  Recontextualize from to (ListTable a) (ListTable b)


instance EqTable a => EqTable (ListTable a) where
  eqTable =
    hvectorize
      (\SSpec {} (Identity dict) -> case dbTypeDict dict of
          Dict -> case dbTypeNullability dict of
            Nullable -> Dict
            NonNullable -> Dict)
      (Identity (eqTable @a))


instance OrdTable a => OrdTable (ListTable a) where
  ordTable =
    hvectorize
      (\SSpec {} (Identity dict) -> case dbTypeDict dict of
          Dict -> case dbTypeNullability dict of
            Nullable -> Dict
            NonNullable -> Dict)
      (Identity (ordTable @a))


instance AltTable ListTable where
  (<|>:) = (<>)


instance AlternativeTable ListTable where
  emptyTable = mempty


instance Table Expr a => Semigroup (ListTable a) where
  ListTable as <> ListTable bs = ListTable $
    happend (\_ _ (DB a) (DB b) -> DB (sappend a b)) as bs


instance Table Expr a => Monoid (ListTable a) where
  mempty = ListTable $ hempty $ \nullability info ->
    DB (sempty nullability info)
