{-# language ApplicativeDo #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}

{-# options -fno-warn-name-shadowing #-}

module Rel8.Optimize where

import Control.Applicative
-- import Control.Arrow ( first )
-- import Data.Foldable ( toList )
import Data.Functor.Identity
-- import Data.List.NonEmpty ( NonEmpty(..) )
-- import qualified Data.Map as M
import Opaleye.Internal.HaskellDB.Sql ( SqlExpr, SqlExpr(..), SqlRangeBound(..) )
import Opaleye.Internal.Sql as O


-- | Traversal suitable for use with @Control.Lens.Plate@. Selects the immediate
-- 'Select's within a 'Select'.
selectPlate
  :: Applicative f
  => ( O.Select -> f O.Select ) -> O.Select -> f O.Select
selectPlate f = \case
  SelectFrom From{..} -> do
    tables <-
      traverse f tables

    return ( SelectFrom From{..} )

  SelectJoin Join{..} -> do
    jTables <-
      both f jTables

    return ( SelectJoin Join{..} )

  SelectBinary Binary{..} -> do
    bSelect1 <-
      f bSelect1

    bSelect2 <-
      f bSelect2

    return ( SelectBinary Binary{..} )

  SelectLabel Label {..} -> do
    lSelect <-
      f lSelect

    return ( SelectLabel Label{..} )

  other ->
    pure other


-- | Traverse all 'SqlExpr's in a 'Select' (but not sub-expressions)
selectExprs
  :: Applicative f
  => ( SqlExpr -> f SqlExpr ) -> O.Select -> f O.Select
selectExprs f = \case
  SelectFrom From{..} -> do
    attrs <-
      traverseAttrs f attrs

    tables <-
      ( traverse . selectExprs ) f  tables

    criteria <-
      traverse f criteria

    groupBy <-
      ( traverse . traverse ) f groupBy

    orderBy <-
      ( traverse . _1 ) f orderBy

    return ( SelectFrom From{..} )

  SelectJoin Join{..} -> do
    jTables <-
      ( both . selectExprs ) f jTables

    jCond <-
      f jCond

    return ( SelectJoin Join{..} )

  SelectBinary Binary {..} -> do
    bSelect1 <-
      selectExprs f bSelect1

    bSelect2 <-
      selectExprs f bSelect2

    return ( SelectBinary Binary{..} )

  SelectLabel Label{..} -> do
    lSelect <-
      selectExprs f lSelect

    return ( SelectLabel Label{..} )

  RelExpr expr ->
    RelExpr <$> f expr

  SelectValues Values{..} -> do
    vAttrs <-
      traverseAttrs f vAttrs

    vValues <-
      ( traverse . traverse ) f vValues

    return ( SelectValues Values{..} )

  other ->
    pure other


-- | Traverse all 'SqlExpr's in 'SelectAttrs'
traverseAttrs
  :: Applicative f
  => ( SqlExpr -> f SqlExpr ) -> SelectAttrs -> f SelectAttrs
traverseAttrs f = \case
  SelectAttrs a ->
    SelectAttrs <$> ( traverse._1 ) f a

  SelectAttrsStar a ->
    SelectAttrsStar <$> ( traverse._1 ) f a

  other ->
    pure other


-- | Traverse both sides of a homogeneous tuple.
both :: Applicative f => ( t -> f b ) -> ( t, t ) -> f ( b, b )
both f ( a,b ) =
  liftA2 (,) ( f a ) ( f b )


-- | Traverse into the first element of a 2-tuple.
_1 :: Applicative f => ( t -> f a ) -> ( t, b ) -> f ( a, b )
_1 f ( a, b ) =
  liftA2 (,) ( f a ) ( pure b )


-- | Traversal suitable for use with @Control.Lens.Plate@. Selects the immediate
-- 'SqlExpr's within a 'SqlExpr'.
traverseExpr :: Applicative f => ( SqlExpr -> f SqlExpr ) -> SqlExpr -> f SqlExpr
traverseExpr f = \case
  CompositeSqlExpr expr s ->
    CompositeSqlExpr <$> f expr <*> pure s

  BinSqlExpr a b c ->
    BinSqlExpr <$> pure a <*> f b <*> f c

  PrefixSqlExpr a b ->
    PrefixSqlExpr <$> pure a <*> f b

  PostfixSqlExpr a b ->
    PostfixSqlExpr <$> pure a <*> f b

  FunSqlExpr a b ->
    FunSqlExpr <$> pure a <*> traverse f b

  AggrFunSqlExpr a b c d ->
    AggrFunSqlExpr <$> pure a <*> traverse f b <*> ( traverse._1 ) f c <*> pure d

  CaseSqlExpr a b ->
    CaseSqlExpr <$> ( traverse.both ) f a <*> f b

  ListSqlExpr b ->
    ListSqlExpr <$> traverse f b

  ParamSqlExpr a b ->
    ParamSqlExpr <$> pure a <*> f b

  ParensSqlExpr a ->
    ParensSqlExpr <$> f a

  CastSqlExpr a b ->
    CastSqlExpr <$> pure a <*> f b

  ArraySqlExpr a ->
    ArraySqlExpr <$> traverse f a

  RangeSqlExpr string a b ->
    RangeSqlExpr string <$> traverseRangeBound a <*> traverseRangeBound b

    where

      traverseRangeBound = \case
        Inclusive e ->
          Inclusive <$> f e

        Exclusive e ->
          Exclusive <$> f e

        other ->
          pure other

  other ->
    pure other



optimiseMaybeJoins :: _ -> _
optimiseMaybeJoins = \case
  CaseExpr [ ( UnExpr OpIsNull x, ConstExpr ( BoolLit False ) ) ] notNullBranch ->
    Just notNullBranch


-- -- --------------------------------------------------------------------------------
-- -- --
-- -- -- Simplify a SELECT query. The following optimisations are applied:
-- -- --
-- -- -- 1. SELECT a, b, c FROM (SELECT .. ) is rewritten to just the inner select.
-- -- --    Any outer predicates are pushed into the inner select.
-- -- --    The outer projections are rewritten in terms of the inner most columns.
-- -- --
-- -- -- 2. SELECT * FROM inner is rewritten to just the inner query. Like (1), but
-- -- --    applies to VALUES expressions.
-- -- --
-- -- --------------------------------------------------------------------------------

-- -- simplifySelect :: O.Select -> O.Select
-- -- simplifySelect =
-- --   unselectRedundantColumns
-- --   . rewriteOf selectPlate (\s -> removeNeedlessSelects s <|> dropOffset0 s)
-- --   . over selectExprs (rewriteOf traverseExpr dropColumnParens)
-- --   . unselectRedundantColumns

-- -- --------------------------------------------------------------------------------
-- -- -- Turns ("a") = ("b") into just "a" = "b".
-- -- dropColumnParens :: SqlExpr -> Maybe SqlExpr
-- -- dropColumnParens (BinSqlExpr op (ParensSqlExpr (ColumnSqlExpr a)) right) =
-- --   Just (BinSqlExpr op (ColumnSqlExpr a) right)
-- -- dropColumnParens (BinSqlExpr op left (ParensSqlExpr (ColumnSqlExpr a))) =
-- --   Just (BinSqlExpr op left (ColumnSqlExpr a))
-- -- dropColumnParens _ = Nothing

-- -- --------------------------------------------------------------------------------
-- -- removeNeedlessSelects :: O.Select -> Maybe O.Select
-- -- removeNeedlessSelects
-- --   (SelectFrom From { attrs = outerAttrs
-- --                    , tables = [SelectFrom inner@From { attrs = innerAttrs }]
-- --                    , criteria = []
-- --                    , groupBy = Nothing
-- --                    , orderBy = []
-- --                    , limit = Nothing
-- --                    , offset = Nothing
-- --                    }) =
-- --   let newAttrs =
-- --         case outerAttrs of
-- --           -- SELECT * FROM (SELECT attrs FROM ...) -> SELECT attrs FROM
-- --           Star ->
-- --             innerAttrs

-- --           SelectAttrs outerAttrs' ->
-- --             case innerAttrs of
-- --               Star ->
-- --                 SelectAttrs outerAttrs'
-- --               SelectAttrs innerAttrs' ->
-- --                 SelectAttrs (fmap (first (substitute innerAttrs')) outerAttrs')
-- --               SelectAttrsStar innerAttrs' ->
-- --                 SelectAttrs (fmap (first (substitute innerAttrs')) outerAttrs')

-- --           SelectAttrsStar outerAttrs' ->
-- --             case innerAttrs of
-- --               Star ->
-- --                 SelectAttrsStar outerAttrs'
-- --               SelectAttrs innerAttrs' ->
-- --                 SelectAttrs (fmap (first (substitute innerAttrs')) outerAttrs' <>
-- --                             innerAttrs')
-- --               SelectAttrsStar innerAttrs' ->
-- --                 SelectAttrsStar (fmap (first (substitute innerAttrs')) outerAttrs' <>
-- --                                   innerAttrs')

-- --   in Just (SelectFrom inner {attrs = newAttrs})

-- -- removeNeedlessSelects _ = Nothing

-- -- --------------------------------------------------------------------------------
-- -- unselectRedundantColumns :: O.Select -> O.Select
-- -- unselectRedundantColumns =
-- --   \s ->
-- --     case s of
-- --       SelectFrom f@From {tables} ->
-- --         SelectFrom f {tables = fmap (compact (dependencies s)) tables}
-- --       other -> other

-- --   where

-- --     dependencies :: O.Select -> S.Set String
-- --     dependencies (SelectFrom From { attrs
-- --                                   , criteria
-- --                                   , groupBy
-- --                                   , orderBy
-- --                                   }) =
-- --       mconcat
-- --         [ foldMap exprReferences (attrExprs attrs)
-- --         , foldMap exprReferences criteria
-- --         , foldMap (foldMap exprReferences) groupBy
-- --         , foldMap (exprReferences . fst) orderBy
-- --         ]
-- --     dependencies (SelectJoin Join{..}) = exprReferences jCond
-- --     dependencies _ = mempty

-- --     attrExprs :: SelectAttrs -> [SqlExpr]
-- --     attrExprs Star = []
-- --     attrExprs (SelectAttrs attrs) = map fst (toList attrs)
-- --     attrExprs (SelectAttrsStar attrs) = map fst (toList attrs)

-- --     exprReferences :: SqlExpr -> S.Set String
-- --     exprReferences (ColumnSqlExpr (SqlColumn a)) = S.singleton a
-- --     exprReferences (CompositeSqlExpr a _) = exprReferences a
-- --     exprReferences (BinSqlExpr _ a b) = exprReferences a <> exprReferences b
-- --     exprReferences (PrefixSqlExpr _ a) = exprReferences a
-- --     exprReferences (PostfixSqlExpr _ a) = exprReferences a
-- --     exprReferences (FunSqlExpr _ args) = foldMap exprReferences args
-- --     exprReferences (ConstSqlExpr _) = mempty
-- --     exprReferences (CaseSqlExpr a b) =
-- --       foldMap exprReferences (fmap fst a) <> foldMap exprReferences (fmap snd a) <>
-- --       exprReferences b
-- --     exprReferences (ListSqlExpr a) = foldMap exprReferences a
-- --     exprReferences (ParamSqlExpr _ a) = exprReferences a
-- --     exprReferences (ParensSqlExpr a) = exprReferences a
-- --     exprReferences (CastSqlExpr _ a) = exprReferences a
-- --     exprReferences (ArraySqlExpr a) = foldMap exprReferences a
-- --     exprReferences (AggrFunSqlExpr _ es ord _) =
-- --       foldMap exprReferences es <>
-- --       foldMap (exprReferences . fst) ord
-- --     exprReferences PlaceHolderSqlExpr = mempty
-- --     exprReferences DefaultSqlExpr = mempty
-- --     exprReferences (RangeSqlExpr a b) = fromRange a <> fromRange b where
-- --       fromRange (Inclusive expr) = exprReferences expr
-- --       fromRange (Exclusive expr) = exprReferences expr
-- --       fromRange _ = mempty

-- --     unColumn (SqlColumn name) = name

-- --     filterAttrsKeeping :: S.Set String -> SelectAttrs -> SelectAttrs
-- --     filterAttrsKeeping _ Star = Star
-- --     filterAttrsKeeping retain (SelectAttrs attrs) =
-- --       case filter (maybe False ((`S.member` retain) . unColumn) . snd) (toList attrs) of
-- --         [] -> SelectAttrs ((ConstSqlExpr "true", Nothing) :| [])
-- --         (a:as) -> SelectAttrs (a :| as)
-- --     filterAttrsKeeping retain (SelectAttrsStar attrs) =
-- --       case filter (maybe False ((`S.member` retain) . unColumn) . snd) (toList attrs) of
-- --         [] -> Star
-- --         (a:as) -> SelectAttrsStar (a :| as)

-- --     compact :: S.Set String -> O.Select -> O.Select
-- --     compact keep (SelectFrom f2@From {attrs, tables}) =
-- --       let reducedAttrs = f2 {attrs = filterAttrsKeeping keep attrs}
-- --           keep' = mappend keep (dependencies (SelectFrom reducedAttrs))
-- --       in SelectFrom reducedAttrs {tables = fmap (compact keep') tables}
-- --     compact keep s@(SelectJoin j@Join {jTables = (l, r)}) =
-- --       let keep' = mappend keep (dependencies s)
-- --       in SelectJoin j {jTables = (compact keep' l, compact keep' r)}
-- --     compact keep s@(SelectBinary b@Binary {bSelect1 = l, bSelect2 = r}) =
-- --       let keep' = mappend keep (dependencies s)
-- --       in SelectBinary b {bSelect1 = compact keep' l, bSelect2 = compact keep' r}
-- --     compact keep s@(SelectLabel l@Label { lSelect }) =
-- --       let keep' = mappend keep (dependencies s)
-- --       in SelectLabel l {lSelect = compact keep' lSelect}
-- --     compact _ other = other


-- -- --------------------------------------------------------------------------------
-- -- dropOffset0 :: O.Select -> Maybe O.Select
-- -- dropOffset0 (SelectFrom f@From {offset = Just 0}) =
-- --   Just (SelectFrom f {offset = Nothing})
-- -- dropOffset0 _ = Nothing

-- -- --------------------------------------------------------------------------------
-- -- --
-- -- -- Given a 'SqlExpr' and environment mapping column names to 'SqlExpr's,
-- -- -- perform substitution of column names to their underlying expressions.
-- -- --
-- -- --------------------------------------------------------------------------------

-- -- substitute
-- --   :: NonEmpty (SqlExpr, Maybe SqlColumn)
-- --   -> SqlExpr
-- --   -> SqlExpr
-- -- substitute exprs expr = transformOf traverseExpr expand expr
-- --   where
-- --     expand :: SqlExpr -> SqlExpr
-- --     expand free@(ColumnSqlExpr (SqlColumn sqlColumn)) =
-- --       case M.lookup sqlColumn env of
-- --         Just a -> a
-- --         Nothing -> free -- Not in the environment, free variable supplied by
-- --                         -- another query?
-- --     expand other = other

-- --     env :: M.Map String SqlExpr
-- --     env =
-- --       M.fromList
-- --         (mapMaybe
-- --            (\(a, mcol) -> fmap (\(SqlColumn str) -> (str, a)) mcol)
-- --            (toList exprs))

transformOf
  :: ( ( a -> Identity a ) -> a -> Identity a )
  -> ( a -> a )
  -> a
  -> a
{-# inline transformOf #-}
transformOf l f = go where

  go =
    f . over l go


rewriteOf
  :: ( ( a -> Identity a ) -> a -> Identity a )
  -> ( a -> Maybe a ) -> a -> a
rewriteOf l f = go where

  go =
    transformOf l ( \x -> maybe x go ( f x ) )


over
  :: ( ( s -> Identity t ) -> a -> Identity b )
  -> ( s -> t )
  -> a
  -> b
over l f =
  runIdentity . l (Identity . f)
