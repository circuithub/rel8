{-# LANGUAGE RecordWildCards #-}
module Rel8.Internal.SqlTransformations where

import Control.Arrow (first)
import Data.Maybe (mapMaybe)
import Control.Lens
import Data.Foldable (toList)
import Opaleye.Internal.Sql as O
import Opaleye.Internal.HaskellDB.Sql
       (SqlExpr, SqlColumn(..), SqlExpr(..), SqlRangeBound(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M

--------------------------------------------------------------------------------

-- | Traversal suitable for use with @Control.Lens.Plate@. Selects the immediate
-- Selects within a Select.

selectPlate :: Traversal' O.Select O.Select
selectPlate f (SelectFrom From {..}) =
  SelectFrom <$>
    (From <$> pure attrs
          <*> traverse f tables             -- < There are some Selects here!
          <*> pure criteria
          <*> pure groupBy
          <*> pure orderBy
          <*> pure limit
          <*> pure offset)
selectPlate f (SelectJoin Join {..}) =
  SelectJoin <$> (Join <$> pure jJoinType
                       <*> both f jTables   -- < More Selects!
                       <*> pure jCond)
selectPlate f (SelectBinary Binary {..}) =
  SelectBinary <$> (Binary <$> pure bOp
                           <*> f bSelect1   -- < Getting the picture?
                           <*> f bSelect2)
selectPlate f (SelectLabel Label {..}) =
  SelectLabel <$> (Label <$> pure lLabel
                         <*> f lSelect)

-- The other constructors in Select don't contain Selects.
selectPlate _ nonRecursive = pure nonRecursive


--------------------------------------------------------------------------------

-- | Traversal suitable for use with @Control.Lens.Plate@. Selects the immediate
-- SqlExprs within a SqlExpr.

traverseExpr :: Traversal' SqlExpr SqlExpr
traverseExpr f (CompositeSqlExpr expr s) = CompositeSqlExpr <$> f expr <*> pure s
traverseExpr f (BinSqlExpr a b c) = BinSqlExpr <$> pure a <*> f b <*> f c
traverseExpr f (PrefixSqlExpr a b) = PrefixSqlExpr <$> pure a <*> f b
traverseExpr f (PostfixSqlExpr a b) = PostfixSqlExpr <$> pure a <*> f b
traverseExpr f (FunSqlExpr a b) = FunSqlExpr <$> pure a <*> traverse f b
traverseExpr f (AggrFunSqlExpr a b c d) = AggrFunSqlExpr <$> pure a <*> traverse f b <*> (traverse._1) f c <*> pure d
traverseExpr f (CaseSqlExpr a b) = CaseSqlExpr <$> (traverse.both) f a <*> f b
traverseExpr f (ListSqlExpr b) = ListSqlExpr <$> traverse f b
traverseExpr f (ParamSqlExpr a b) = ParamSqlExpr <$> pure a <*> f b
traverseExpr f (ParensSqlExpr a) = ParensSqlExpr <$> f a
traverseExpr f (CastSqlExpr a b) = CastSqlExpr <$> pure a <*> f b
traverseExpr f (ArraySqlExpr a) = ArraySqlExpr <$> traverse f a
traverseExpr f (RangeSqlExpr a b) =
  RangeSqlExpr <$> traverseRangeBound a <*> traverseRangeBound b
  where
    traverseRangeBound (Inclusive e) = Inclusive <$> f e
    traverseRangeBound (Exclusive e) = Exclusive <$> f e
    traverseRangeBound other = pure other
traverseExpr _ other = pure other


--------------------------------------------------------------------------------

-- Simplify a SELECT query. The following optimisations are applied:
--
-- 1. SELECT a, b, c FROM (SELECT .. ) is rewritten to just the inner select.
--    Any outer predicates are pushed into the inner select.
--    The outer projections are rewritten in terms of the inner most columns.
--
-- 2. SELECT * FROM inner is rewritten to just the inner query. Like (1), but
--    applies to VALUES expressions.

simplifySelect :: O.Select -> O.Select
simplifySelect = transformOf selectPlate go
  where
    go :: O.Select -> O.Select

    go (SelectFrom From { attrs = outerAttrs
                        , tables = [SelectFrom inner@From { attrs = innerAttrs
                                                          , criteria = c2
                                                          }]
                        , criteria = c1
                        , groupBy = Nothing
                        , orderBy = []
                        , limit = Nothing
                        , offset = Nothing
                        }) =
      let newAttrs =
            case (outerAttrs, innerAttrs) of
              (SelectAttrs outerAttrs', SelectAttrs innerAttrs') ->
                SelectAttrs (fmap (first (rename innerAttrs')) outerAttrs')
              (Star, x) -> x
      in SelectFrom inner {attrs = newAttrs, criteria = c1 ++ c2}

    go (SelectFrom From { attrs = Star
                        , tables = [t]
                        , criteria = []
                        , groupBy = Nothing
                        , orderBy = []
                        , limit = Nothing
                        , offset = Nothing
                        }) = t
    go retain = retain


--------------------------------------------------------------------------------

--
-- Given a 'SqlExpr' and environment mapping column names to 'SqlExpr's,
-- perform substitution of column names to their underlying expressions.

rename
  :: NonEmpty (SqlExpr, Maybe SqlColumn)
  -> SqlExpr
  -> SqlExpr
rename exprs expr = transformOf traverseExpr expand expr
  where
    expand free@(ColumnSqlExpr (SqlColumn sqlColumn)) =
      case M.lookup sqlColumn env of
        Just a -> a
        Nothing -> free -- Not in the environment, free variable supplied by
                        -- another query?
    expand other = other
    env =
      M.fromList
        (mapMaybe
           (\(a, mcol) -> fmap (\(SqlColumn str) -> (str, a)) mcol)
           (toList exprs))
