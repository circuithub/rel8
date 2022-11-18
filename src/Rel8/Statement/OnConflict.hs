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
  , Upsert(..)
  , ppOnConflict
  )
where

-- base
import Data.Foldable ( toList )
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
import Rel8.Schema.Name ( Name, Selects, ppColumn )
import Rel8.Schema.Table ( TableSchema(..) )
import Rel8.Statement.Set ( ppSet )
import Rel8.Statement.Where ( ppWhere )
import Rel8.Table ( Table, toColumns )
import Rel8.Table.Cols ( Cols( Cols ) )
import Rel8.Table.Name ( showNames )
import Rel8.Table.Opaleye (attributes, view)
import Rel8.Table.Projection ( Projecting, Projection, apply )


-- | 'OnConflict' represents the @ON CONFLICT@ clause of an @INSERT@
-- statement. This specifies what ought to happen when one or more of the
-- rows proposed for insertion conflict with an existing row in the table.
type OnConflict :: Type -> Type
data OnConflict names
  = Abort
    -- ^ Abort the transaction if there are conflicting rows (Postgres' default)
  | DoNothing
    -- ^ @ON CONFLICT DO NOTHING@
  | DoUpdate (Upsert names)
    -- ^ @ON CONFLICT DO UPDATE@


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
data Upsert names where
  Upsert :: (Selects names exprs, Projecting names index, excluded ~ exprs) =>
    { index :: Projection names index
      -- ^ The set of columns comprising the @UNIQUE@ index that forms our
      -- conflict target, projected from the set of columns for the whole
      -- table
    , predicate :: Maybe (exprs -> Expr Bool)
      -- ^ An optional predicate used to specify a
      -- [partial index](https://www.postgresql.org/docs/current/indexes-partial.html).
    , set :: excluded -> exprs -> exprs
      -- ^ How to update each selected row.
    , updateWhere :: excluded -> exprs -> Expr Bool
      -- ^ Which rows to select for update.
    }
    -> Upsert names
  PartialUpsert :: (Selects names exprs, Projecting names index, excluded ~ exprs) =>
    { index :: Projection names index
      -- ^ The set of conflict targets, projected from the set of columns for
      -- the whole table
    , indexWhere :: exprs -> Expr Bool
      -- ^ Index predicate for partial index support
    , set :: excluded -> exprs -> exprs
      -- ^ How to update each selected row.
    , updateWhere :: excluded -> exprs -> Expr Bool
      -- ^ Which rows to select for update.
    }
    -> Upsert names


ppOnConflict :: TableSchema names -> OnConflict names -> Doc
ppOnConflict schema = \case
  Abort -> mempty
  DoNothing -> text "ON CONFLICT DO NOTHING"
  DoUpdate upsert -> ppUpsert schema upsert


ppUpsert :: TableSchema names -> Upsert names -> Doc
ppUpsert schema@TableSchema {columns} Upsert {..} =
  text "ON CONFLICT" <+>
  ppIndex columns index <+> foldMap (ppPredicate columns) predicate <+>
  text "DO UPDATE" $$
  ppSet schema (set excluded) $$
  ppWhere schema (updateWhere excluded)
  where
    excluded = attributes TableSchema
      { name = "excluded"
      , columns
      }
ppUpsert schema@TableSchema {columns} PartialUpsert {..} =
  text "ON CONFLICT" <+>
  ppIndex columns index <+>
  ppWhere schema indexWhere <+>
  text "DO UPDATE" $$
  ppSet schema (set excluded) $$
  ppWhere schema (updateWhere excluded)
  where
    excluded = attributes TableSchema
      { name = "excluded"
      , columns
      }


ppIndex :: (Table Name names, Projecting names index)
  => names -> Projection names index -> Doc
ppIndex columns index =
  parens $ Opaleye.commaV ppColumn $ toList $
    showNames $ Cols $ apply index $ toColumns columns


ppPredicate :: Selects names exprs
  => names -> (exprs -> Expr Bool) -> Doc
ppPredicate schema where_ = text "WHERE" <+> ppExpr condition
  where
    ppExpr = Opaleye.ppSqlExpr . Opaleye.sqlExpr . toPrimExpr
    condition = where_ (view schema)
