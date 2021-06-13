{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table.List
  ( ListTable(..)
  , listTable, insertListTable, nameListTable
  )
where

-- base
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Type )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude

-- rel8
import Rel8.Expr ( Expr, Col( E, unE ) )
import Rel8.Expr.Array ( sappend, sempty, slistOf )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable.List ( HListTable )
import Rel8.Schema.HTable.Vectorize ( happend, hempty, hvectorize )
import Rel8.Schema.Insert ( Inserts )
import Rel8.Schema.Name ( Col( N ), Name( Name ) )
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Schema.Spec ( SSpec(..) )
import Rel8.Schema.Spec.ConstrainDBType ( dbTypeDict, dbTypeNullity )
import Rel8.Schema.Reify ( hreify, hunreify )
import Rel8.Table
  ( Table, Context, Columns, fromColumns, toColumns
  , reify, unreify
  )
import Rel8.Table.Alternative
  ( AltTable, (<|>:)
  , AlternativeTable, emptyTable
  )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Insert ( toInsert )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Serialize ( FromExprs, ToExprs, fromResult, toResult )
import Rel8.Table.Unreify ( Unreifies )


-- | A @ListTable@ value contains zero or more instances of @a@. You construct
-- @ListTable@s with 'Rel8.many' or 'Rel8.listAgg'.
type ListTable :: Type -> Type
newtype ListTable a = ListTable (HListTable (Columns a) (Col (Context a)))


instance (Table context a, Unreifies context a) =>
  Table context (ListTable a)
 where
  type Columns (ListTable a) = HListTable (Columns a)
  type Context (ListTable a) = Context a

  fromColumns = ListTable
  toColumns (ListTable a) = a

  reify Refl (ListTable a) = ListTable (hreify a)
  unreify Refl (ListTable a) = ListTable (hunreify a)


instance
  ( Unreifies from a, Unreifies to b
  , Recontextualize from to a b
  )
  => Recontextualize from to (ListTable a) (ListTable b)


instance EqTable a => EqTable (ListTable a) where
  eqTable =
    hvectorize
      (\SSpec {} (Identity dict) -> case dbTypeDict dict of
          Dict -> case dbTypeNullity dict of
            Null -> Dict
            NotNull -> Dict)
      (Identity (eqTable @a))


instance OrdTable a => OrdTable (ListTable a) where
  ordTable =
    hvectorize
      (\SSpec {} (Identity dict) -> case dbTypeDict dict of
          Dict -> case dbTypeNullity dict of
            Null -> Dict
            NotNull -> Dict)
      (Identity (ordTable @a))


type instance FromExprs (ListTable a) = [FromExprs a]


instance ToExprs exprs a => ToExprs (ListTable exprs) [a] where
  fromResult = fmap (fromResult @exprs) . fromColumns
  toResult = toColumns . fmap (toResult @exprs)


instance AltTable ListTable where
  (<|>:) = (<>)


instance AlternativeTable ListTable where
  emptyTable = mempty


instance Table Expr a => Semigroup (ListTable a) where
  ListTable as <> ListTable bs = ListTable $
    happend (\_ _ (E a) (E b) -> E (sappend a b)) as bs


instance Table Expr a => Monoid (ListTable a) where
  mempty = ListTable $ hempty $ \nullability info ->
    E (sempty nullability info)


listTable :: Table Expr a => [a] -> ListTable a
listTable =
  ListTable .
  hvectorize (\SSpec {info} -> E . slistOf info . fmap unE) .
  fmap toColumns


insertListTable :: Inserts exprs inserts => [exprs] -> ListTable inserts
insertListTable = toInsert . listTable


nameListTable :: Table Name a => a -> ListTable a
nameListTable =
  ListTable .
  hvectorize (\_ (Identity (N (Name a))) -> N (Name a)) .
  pure .
  toColumns
