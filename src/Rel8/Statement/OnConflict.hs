{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}
{-# language TypeOperators #-}

module Rel8.Statement.OnConflict
  ( OnConflict(..)
  , Conflict (..)
  , Index (..)
  , Upsert(..)
  , ppOnConflict
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye
import qualified Opaleye.Internal.Sql as Opaleye

-- pretty
import Text.PrettyPrint ( Doc, (<+>), ($$), parens, text )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye (toPrimExpr)
import Rel8.Schema.Escape (escape)
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.HTable (hfoldMap)
import Rel8.Schema.Table ( TableSchema(..) )
import Rel8.Statement.Set ( ppSet )
import Rel8.Statement.Where ( ppWhere )
import Rel8.Table ( Table, toColumns )
import Rel8.Table.Opaleye (attributes, view)


-- | 'OnConflict' represents the @ON CONFLICT@ clause of an @INSERT@
-- statement. This specifies what ought to happen when one or more of the
-- rows proposed for insertion conflict with an existing row in the table.
type OnConflict :: Type -> Type
data OnConflict exprs
  = Abort
    -- ^ Abort the transaction if there are conflicting rows (Postgres' default)
  | DoNothing (Maybe (Conflict exprs))
    -- ^ @ON CONFLICT DO NOTHING@, or @ON CONFLICT (...) DO NOTHING@ if an
    -- explicit conflict target  is supplied. Specifying a conflict target is
    -- essential when your table has has deferrable constraints — @ON
    -- CONFLICT@ can't work on deferrable constraints, so it's necessary
    -- to explicitly name one of its non-deferrable constraints in order to
    -- use @ON CONFLICT@.
  | DoUpdate (Upsert exprs)
    -- ^ @ON CONFLICT (...) DO UPDATE ...@


-- | The @ON CONFLICT (...) DO UPDATE@ clause of an @INSERT@ statement, also
-- known as \"upsert\".
--
-- When an existing row conflicts with a row proposed for insertion,
-- @ON CONFLICT DO UPDATE@ allows you to instead update this existing row. The
-- conflicting row proposed for insertion is then \"excluded\", but its values
-- can still be referenced from the @SET@ and @WHERE@ clauses of the @UPDATE@
-- statement.
--
-- Upsert in Postgres a \"conflict target\" to be specified — this is the
-- @UNIQUE@ index from conflicts with which we would like to recover. Indexes
-- are specified by listing the columns that comprise them along with an
-- optional predicate in the case of partial indexes.
type Upsert :: Type -> Type
data Upsert exprs where
  Upsert :: excluded ~ exprs =>
    { conflict :: Conflict exprs
      -- ^ The conflict target to supply to @DO UPDATE@.
    , set :: excluded -> exprs -> exprs
      -- ^ How to update each selected row.
    , updateWhere :: excluded -> exprs -> Expr Bool
      -- ^ Which rows to select for update.
    }
    -> Upsert exprs


-- | Represents what PostgreSQL calls a
-- [@conflict_target@](https://www.postgresql.org/docs/current/sql-insert.html#SQL-ON-CONFLICT)
-- in an @ON CONFLICT@ clause of an @INSERT@ statement.
type Conflict :: Type -> Type
data Conflict exprs
  = OnConstraint String
  -- ^ Use a specific named constraint for the conflict target. This
  -- corresponds the the syntax @ON CONFLICT constraint@ in PostgreSQL.
  | OnIndex (Index exprs)
  -- ^ Have PostgreSQL perform what it calls _unique index inference_ by
  -- giving it a description of the target index.


-- | A description of the target unique index — its columns (and/or
-- expressions) and, in the case of partial indexes, a predicate.
type Index :: Type -> Type
data Index exprs where
  Index :: Table Expr index =>
    { columns :: exprs -> index
      -- ^ The set of columns and/or expressions comprising the @UNIQUE@ index
    , predicate :: Maybe (exprs -> Expr Bool)
      -- ^ An optional predicate used to specify a
      -- [partial index](https://www.postgresql.org/docs/current/indexes-partial.html).
    }
    -> Index exprs


ppOnConflict :: Selects names exprs => TableSchema names -> OnConflict exprs -> Doc
ppOnConflict schema@TableSchema {columns} = \case
  Abort -> mempty
  DoNothing conflict -> text "ON CONFLICT" <+> foldMap (ppConflict row) conflict <+> text "DO NOTHING"
  DoUpdate upsert -> ppUpsert schema row upsert
  where
    row = view columns


ppConflict :: exprs -> Conflict exprs -> Doc
ppConflict row = \case
  OnConstraint name -> "ON CONSTRAINT" <+> escape name
  OnIndex index -> ppIndex row index


ppIndex :: exprs -> Index exprs -> Doc
ppIndex row Index {columns, predicate} =
  parens (Opaleye.commaH id exprs) <>
  foldMap (ppPredicate . ($ row)) predicate
  where
    exprs = hfoldMap (pure . parens . ppExpr) $ toColumns $ columns row


ppPredicate :: Expr Bool -> Doc
ppPredicate condition = text "WHERE" <+> ppExpr condition


ppUpsert :: Selects names exprs => TableSchema names -> exprs -> Upsert exprs -> Doc
ppUpsert schema@TableSchema {columns} row Upsert {..} =
  text "ON CONFLICT" <+> ppConflict row conflict <+> "DO UPDATE" $$
  ppSet schema (set excluded) $$
  ppWhere schema (updateWhere excluded)
  where
    excluded = attributes TableSchema
      { name = "excluded"
      , columns
      }


ppExpr :: Expr a -> Doc
ppExpr = Opaleye.ppSqlExpr . Opaleye.sqlExpr . toPrimExpr
