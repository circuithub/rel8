{-# language GADTs #-}
{-# language LambdaCase #-}

module SQL where

import Data.List
import Data.Void ( Void )
import Numeric.Natural ( Natural )

type TableName = String

type Alias = String

type Expr = String

type ColumnAlias = String

data Query one where
  -- | The one-row query of 0 columns. This acts as an identity to any query.
  One :: one -> Query one

  -- | Select a table and give it a particular alias
  SelectTable :: TableName -> Alias -> Query one

  -- | Project a subset of columns from a query
  Project
    :: [ ( Expr, ColumnAlias ) ]
    -> Query one
    -> Query one

  -- | Introduce a WHERE clause
  Where :: Expr -> Query one -> Query one

  -- | Introduce a LIMIT clause
  Limit :: Natural -> Query one -> Query one

  -- | Introduce an OFFSET clause
  Offset :: Natural -> Query one -> Query one

  -- | Compute the cartesian product of two tables
  Product :: Query one -> Query one -> Query one

  -- | Introduce an ORDER BY clause
  Order :: [ Expr ] -> Query one -> Query one


-- | Given a 'Query', try and find an equivalent
-- 'Query' that doesn't need 'One'.
withoutOne :: Query () -> Maybe ( Query Void )
withoutOne = \case
  One _ ->
    Nothing

  SelectTable a b ->
    Just ( SelectTable a b )

  Product One{} x ->
    withoutOne x

  Product x One{} ->
    withoutOne x

  Product x y -> do
    x' <-
      withoutOne x

    y' <-
      withoutOne y

    return ( Product x' y' )

  Project exprs q ->
    Project exprs <$> withoutOne q


renderQuery :: Query Void -> String
renderQuery = \case
  SelectTable t alias ->
    "SELECT * FROM " ++ t ++ " " ++ alias

  Project exprs q ->
    "SELECT "
      ++ intercalate
           ", "
           ( map
               ( \( expr, alias ) ->
                   expr ++ " " ++ alias
               )
               exprs
           )
      ++ " FROM ("
      ++ renderQuery q
      ++ ")"
