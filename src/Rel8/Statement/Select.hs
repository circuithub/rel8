{-# language DataKinds #-}
{-# language DeriveTraversable #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

module Rel8.Statement.Select
  ( select
  , ppSelect
  , Optimized(..)
  , ppPrimSelect
  , ppRows
  )
where

-- base
import Data.Foldable ( toList )
import Data.Kind ( Type )
import Data.Void ( Void )
import Prelude hiding ( undefined )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.HaskellDB.Sql as Opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.Print as Opaleye
import qualified Opaleye.Internal.Optimize as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye hiding ( Select )
import qualified Opaleye.Internal.Sql as Opaleye hiding ( Values )
import qualified Opaleye.Internal.Tag as Opaleye

-- pretty
import Text.PrettyPrint ( Doc )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( false )
import Rel8.Expr.Opaleye ( toPrimExpr )
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( toOpaleye )
import Rel8.Schema.Name ( Selects )
import Rel8.Statement (Statement, statementReturning)
import Rel8.Table ( Table )
import Rel8.Table.Cols ( toCols )
import Rel8.Table.Name ( namesFromLabels )
import Rel8.Table.Opaleye ( castTable, exprsWithNames )
import qualified Rel8.Table.Opaleye as T
import Rel8.Table.Undefined ( undefined )

-- transformers
import Control.Monad.Trans.State.Strict (State)


-- | Build a @SELECT@ 'Statement'.
select :: Table Expr a => Query a -> Statement (Query a)
select query = statementReturning (ppSelect query)


ppSelect :: Table Expr a => Query a -> State Opaleye.Tag Doc
ppSelect query = do
  (exprs, primQuery) <- Opaleye.runSimpleSelect (toOpaleye query)
  let
    (exprs', primQuery') = case optimize primQuery of
      Empty -> (undefined, Opaleye.Product (pure (pure Opaleye.Unit)) never)
      Unit -> (exprs, Opaleye.Unit)
      Optimized pq -> (exprs, pq)
  pure $ Opaleye.ppSql $ primSelectWith names (toCols exprs') primQuery'
  where
    names = namesFromLabels
    never = pure (toPrimExpr false)


ppRows :: Table Expr a => Query a -> State Opaleye.Tag Doc
ppRows query = case optimize primQuery of
  -- Special case VALUES because we can't use DEFAULT inside a SELECT
  Optimized (Opaleye.Values symbols rows)
    | eqSymbols symbols (toList (T.exprs a)) ->
        pure $ Opaleye.ppValues_ (map Opaleye.sqlExpr <$> toList rows)
  _ -> ppSelect query
  where
    (a, primQuery, _) = Opaleye.runSimpleQueryArrStart (toOpaleye query) ()

    eqSymbols (symbol : symbols) (Opaleye.AttrExpr symbol' : exprs)
      | eqSymbol symbol symbol' = eqSymbols symbols exprs
      | otherwise = False
    eqSymbols [] [] = True
    eqSymbols _ _ = False

    eqSymbol
      (Opaleye.Symbol name (Opaleye.UnsafeTag tag))
      (Opaleye.Symbol name' (Opaleye.UnsafeTag tag'))
      = name == name' && tag == tag'


ppPrimSelect :: Query a -> State Opaleye.Tag (Optimized Doc, a)
ppPrimSelect query = do
  (a, primQuery) <- Opaleye.runSimpleSelect (toOpaleye query)
  pure $ (Opaleye.ppSql . primSelect <$> optimize primQuery, a)


type Optimized :: Type -> Type
data Optimized a = Empty | Unit | Optimized a
  deriving stock (Functor, Foldable, Traversable, Show)


optimize :: Opaleye.PrimQuery' a -> Optimized (Opaleye.PrimQuery' Void)
optimize query = case Opaleye.removeEmpty (Opaleye.optimize query) of
  Nothing -> Empty
  Just Opaleye.Unit -> Unit
  Just query' -> Optimized query'


primSelect :: Opaleye.PrimQuery' Void -> Opaleye.Select
primSelect = Opaleye.foldPrimQuery Opaleye.sqlQueryGenerator


primSelectWith :: Selects names exprs
  => names -> exprs -> Opaleye.PrimQuery' Void -> Opaleye.Select
primSelectWith names exprs query =
  Opaleye.SelectFrom $ Opaleye.newSelect
    { Opaleye.attrs = Opaleye.SelectAttrs attrs
    , Opaleye.tables = Opaleye.oneTable (primSelect query)
    }
  where
    attrs = makeAttr <$> exprsWithNames names (castTable exprs)
      where
        makeAttr (label, expr) =
          (Opaleye.sqlExpr expr, Just (Opaleye.SqlColumn label))
