{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}
{-# language TypeApplications #-}

module Rel8.Statement.Returning
  ( Returning( NoReturning, Returning )
  , ppReturning
  )
where

-- base
import Data.Foldable ( toList )
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye
import qualified Opaleye.Internal.Sql as Opaleye

-- pretty
import Text.PrettyPrint ( Doc, (<+>), text )

-- rel8
import Rel8.Expr (Expr)
import Rel8.Query ( Query )
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema(..) )
import Rel8.Table (Table)
import Rel8.Table.Opaleye ( castTable, exprs, view )


-- | 'Rel8.Insert', 'Rel8.Update' and 'Rel8.Delete' all support an optional
-- @RETURNING@ clause.
type Returning :: Type -> Type -> Type
data Returning names a where
  -- | No @RETURNING@ clause
  NoReturning :: Returning names ()

  -- | 'Returning' allows you to project out of the affected rows, which can
  -- be useful if you want to log exactly which rows were deleted, or to view
  -- a generated id (for example, if using a column with an autoincrementing
  -- counter via 'Rel8.nextval').
  Returning :: (Selects names exprs, Table Expr a) => (exprs -> a) -> Returning names (Query a)


projections :: ()
  => TableSchema names -> Returning names a -> Maybe (NonEmpty Opaleye.PrimExpr)
projections TableSchema {columns} = \case
  NoReturning -> Nothing
  Returning f -> Just (exprs (castTable (f (view columns))))


ppReturning :: TableSchema names -> Returning names a -> Doc
ppReturning schema returning = case projections schema returning of
  Nothing -> mempty
  Just columns ->
    text "RETURNING" <+> Opaleye.commaV Opaleye.ppSqlExpr (toList sqlExprs)
    where
      sqlExprs = Opaleye.sqlExpr <$> columns
