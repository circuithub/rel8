{-# language DataKinds #-}
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
  , listTable, nameListTable
  )
where

-- base
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Expr ( Expr( E, unE ) )
import Rel8.Expr.Array ( sappend, sempty, slistOf )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable.List ( HListTable )
import Rel8.Schema.HTable.Vectorize ( happend, hempty, hvectorize, hunvectorize )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name( N, Name ) )
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Schema.Result ( vectorizer, unvectorizer )
import Rel8.Schema.Spec ( SSpec(..) )
import Rel8.Table
  ( Table, Context, Columns, fromColumns, toColumns
  , FromExprs, fromResult, toResult
  )
import Rel8.Table.Alternative
  ( AltTable, (<|>:)
  , AlternativeTable, emptyTable
  )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Serialize ( ToExprs )


-- | A @ListTable@ value contains zero or more instances of @a@. You construct
-- @ListTable@s with 'Rel8.many' or 'Rel8.listAgg'.
type ListTable :: K.Context -> Type -> Type
newtype ListTable context a =
  ListTable (HListTable (Columns a) (Context a))


instance (Table context a, context ~ context') =>
  Table context' (ListTable context a)
 where
  type Columns (ListTable context a) = HListTable (Columns a)
  type Context (ListTable context a) = Context a
  type FromExprs (ListTable context a) = [FromExprs a]

  fromColumns = ListTable
  toColumns (ListTable a) = a
  fromResult = fmap (fromResult @_ @a) . hunvectorize unvectorizer
  toResult = hvectorize vectorizer . fmap (toResult @_ @a)


instance (Recontextualize from to a b, from ~ from', to ~ to') =>
  Recontextualize from to (ListTable from' a) (ListTable to' b)


instance (EqTable a, context ~ Expr) => EqTable (ListTable context a) where
  eqTable =
    hvectorize
      (\SSpec {nullity} (Identity Dict) -> case nullity of
        Null -> Dict
        NotNull -> Dict)
      (Identity (eqTable @a))


instance (OrdTable a, context ~ Expr) => OrdTable (ListTable context a) where
  ordTable =
    hvectorize
      (\SSpec {nullity} (Identity Dict) -> case nullity of
        Null -> Dict
        NotNull -> Dict)
      (Identity (ordTable @a))


instance (ToExprs exprs a, context ~ Expr) =>
  ToExprs (ListTable context exprs) [a]


instance context ~ Expr => AltTable (ListTable context) where
  (<|>:) = (<>)


instance context ~ Expr => AlternativeTable (ListTable context) where
  emptyTable = mempty


instance (context ~ Expr, Table Expr a) =>
  Semigroup (ListTable context a)
 where
  ListTable as <> ListTable bs = ListTable $
    happend (\_ _ (E a) (E b) -> E (sappend a b)) as bs


instance (context ~ Expr, Table Expr a) =>
  Monoid (ListTable context a)
 where
  mempty = ListTable $ hempty $ \_ -> E . sempty


-- | Construct a @ListTable@ from a list of expressions.
listTable :: Table Expr a => [a] -> ListTable Expr a
listTable =
  ListTable .
  hvectorize (\SSpec {info} -> E . slistOf info . fmap unE) .
  fmap toColumns


-- | Construct a 'ListTable' in the 'Name' context. This can be useful if you
-- have a 'ListTable' that you are storing in a table and need to construct a
-- 'TableSchema'.
nameListTable
  :: Table Name a
  => a -- ^ The names of the columns of elements of the list.
  -> ListTable Name a
nameListTable =
  ListTable .
  hvectorize (\_ (Identity (N (Name a))) -> N (Name a)) .
  pure .
  toColumns
