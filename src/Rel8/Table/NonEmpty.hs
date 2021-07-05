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

module Rel8.Table.NonEmpty
  ( NonEmptyTable(..)
  , nonEmptyTable, nameNonEmptyTable
  )
where

-- base
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty )
import Prelude hiding ( id )

-- rel8
import Rel8.Expr ( Expr, Col( E, unE ) )
import Rel8.Expr.Array ( sappend1, snonEmptyOf )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable.NonEmpty ( HNonEmptyTable )
import Rel8.Schema.HTable.Vectorize ( happend, hvectorize, hunvectorize )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Col( N ), Name( Name ) )
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Schema.Result ( vectorizer, unvectorizer )
import Rel8.Schema.Spec ( SSpec(..) )
import Rel8.Table
  ( Table, Context, Columns, fromColumns, toColumns
  , FromExprs, fromResult, toResult
  )
import Rel8.Table.Alternative ( AltTable, (<|>:) )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Serialize ( ToExprs )


-- | A @NonEmptyTable@ value contains one or more instances of @a@. You
-- construct @NonEmptyTable@s with 'Rel8.some' or 'nonEmptyAgg'.
type NonEmptyTable :: K.Context -> Type -> Type
newtype NonEmptyTable context a =
  NonEmptyTable (HNonEmptyTable (Columns a) (Col (Context a)))


instance (Table context a, context ~ context') =>
  Table context' (NonEmptyTable context a)
 where
  type Columns (NonEmptyTable context a) = HNonEmptyTable (Columns a)
  type Context (NonEmptyTable context a) = Context a
  type FromExprs (NonEmptyTable context a) = NonEmpty (FromExprs a)

  fromColumns = NonEmptyTable
  toColumns (NonEmptyTable a) = a
  fromResult = fmap (fromResult @_ @a) . hunvectorize unvectorizer
  toResult = hvectorize vectorizer . fmap (toResult @_ @a)


instance (Recontextualize from to a b, from ~ from', to ~ to') =>
  Recontextualize from to (NonEmptyTable from' a) (NonEmptyTable to' b)


instance (EqTable a, context ~ Expr) =>
  EqTable (NonEmptyTable context a)
 where
  eqTable =
    hvectorize
      (\SSpec {nullity} (Identity Dict) -> case nullity of
        Null -> Dict
        NotNull -> Dict)
      (Identity (eqTable @a))


instance (OrdTable a, context ~ Expr) =>
  OrdTable (NonEmptyTable context a)
 where
  ordTable =
    hvectorize
      (\SSpec {nullity} (Identity Dict) -> case nullity of
        Null -> Dict
        NotNull -> Dict)
      (Identity (ordTable @a))


instance (ToExprs exprs a, context ~ Expr) =>
  ToExprs (NonEmptyTable context exprs) (NonEmpty a)


instance context ~ Expr => AltTable (NonEmptyTable context) where
  (<|>:) = (<>)


instance (Table Expr a, context ~ Expr) =>
  Semigroup (NonEmptyTable context a)
 where
  NonEmptyTable as <> NonEmptyTable bs = NonEmptyTable $
    happend (\_ _ (E a) (E b) -> E (sappend1 a b)) as bs


-- | Construct a @NonEmptyTable@ from a non-empty list of expressions.
nonEmptyTable :: Table Expr a => NonEmpty a -> NonEmptyTable Expr a
nonEmptyTable =
  NonEmptyTable .
  hvectorize (\SSpec {info} -> E . snonEmptyOf info . fmap unE) .
  fmap toColumns


-- | Construct a 'NonEmptyTable' in the 'Name' context. This can be useful if
-- you have a 'NonEmptyTable' that you are storing in a table and need to
-- construct a 'TableSchema'.
nameNonEmptyTable
  :: Table Name a
  => a -- ^ The names of the columns of elements of the list.
  -> NonEmptyTable Name a
nameNonEmptyTable =
  NonEmptyTable .
  hvectorize (\_ (Identity (N (Name a))) -> N (Name a)) .
  pure .
  toColumns
