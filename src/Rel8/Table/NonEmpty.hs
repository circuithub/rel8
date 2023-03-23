{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Table.NonEmpty
  ( NonEmptyTable(..)
  , ($+)
  , nonEmptyTable
  , nameNonEmptyTable
  )
where

-- base
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty )
import Prelude hiding ( id )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Array ( sappend1, snonEmptyOf )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable.NonEmpty ( HNonEmptyTable )
import Rel8.Schema.HTable.Vectorize
  ( hvectorize, hunvectorize
  , happend
  , hproject, hcolumn
  )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name( Name ) )
import Rel8.Schema.Null ( Nullity( Null, NotNull ) )
import Rel8.Schema.Result ( vectorizer, unvectorizer )
import Rel8.Schema.Spec ( Spec(..) )
import Rel8.Table
  ( Table, Context, Columns, fromColumns, toColumns
  , FromExprs, fromResult, toResult
  , Transpose
  )
import Rel8.Table.Alternative ( AltTable, (<|>:) )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Projection
  ( Projectable, Projecting, Projection, project, apply
  )
import Rel8.Table.Serialize ( ToExprs )


-- | A @NonEmptyTable@ value contains one or more instances of @a@. You
-- construct @NonEmptyTable@s with 'Rel8.some' or 'nonEmptyAgg'.
type NonEmptyTable :: K.Context -> Type -> Type
newtype NonEmptyTable context a =
  NonEmptyTable (HNonEmptyTable (Columns a) (Context a))


instance Projectable (NonEmptyTable context) where
  project f (NonEmptyTable a) = NonEmptyTable (hproject (apply f) a)


instance (Table context a, context ~ context') =>
  Table context' (NonEmptyTable context a)
 where
  type Columns (NonEmptyTable context a) = HNonEmptyTable (Columns a)
  type Context (NonEmptyTable context a) = Context a
  type FromExprs (NonEmptyTable context a) = NonEmpty (FromExprs a)
  type Transpose to (NonEmptyTable context a) =
    NonEmptyTable to (Transpose to a)

  fromColumns = NonEmptyTable
  toColumns (NonEmptyTable a) = a
  fromResult = fmap (fromResult @_ @a) . hunvectorize unvectorizer
  toResult = hvectorize vectorizer . fmap (toResult @_ @a)


instance (EqTable a, context ~ Expr) =>
  EqTable (NonEmptyTable context a)
 where
  eqTable =
    hvectorize
      (\Spec {nullity} (Identity Dict) -> case nullity of
        Null -> Dict
        NotNull -> Dict)
      (Identity (eqTable @a))


instance (OrdTable a, context ~ Expr) =>
  OrdTable (NonEmptyTable context a)
 where
  ordTable =
    hvectorize
      (\Spec {nullity} (Identity Dict) -> case nullity of
        Null -> Dict
        NotNull -> Dict)
      (Identity (ordTable @a))


instance (ToExprs exprs a, context ~ Expr) =>
  ToExprs (NonEmptyTable context exprs) (NonEmpty a)


instance context ~ Expr => AltTable (NonEmptyTable context) where
  (<|>:) = (<>)


instance (Table Expr a, context ~ Expr) => Semigroup (NonEmptyTable context a)
 where
  NonEmptyTable as <> NonEmptyTable bs = NonEmptyTable $
    happend (const sappend1) as bs


-- | Project a single expression out of a 'NonEmptyTable'.
($+) :: Projecting a (Expr b)
  => Projection a (Expr b) -> NonEmptyTable Expr a -> Expr (NonEmpty b)
f $+ NonEmptyTable a = hcolumn $ hproject (apply f) a
infixl 4 $+


-- | Construct a @NonEmptyTable@ from a non-empty list of expressions.
nonEmptyTable :: Table Expr a => NonEmpty a -> NonEmptyTable Expr a
nonEmptyTable =
  NonEmptyTable .
  hvectorize (\Spec {info} -> snonEmptyOf info) .
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
  hvectorize (\_ (Identity (Name a)) -> Name a) .
  pure .
  toColumns
