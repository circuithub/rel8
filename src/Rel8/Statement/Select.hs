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
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Data.Void ( Void )
import Prelude hiding ( undefined )

-- hasql
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Statement as Hasql

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
import Rel8.Table ( Table )
import Rel8.Table.Cols ( toCols )
import Rel8.Table.Name ( namesFromLabels )
import Rel8.Table.Opaleye ( castTable, exprsWithNames )
import qualified Rel8.Table.Opaleye as T
import Rel8.Table.Serialize ( Serializable, parse )
import Rel8.Table.Undefined ( undefined )

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )


-- | Run a @SELECT@ statement, returning all rows.
select :: forall exprs a. Serializable exprs a
  => Query exprs -> Hasql.Statement () [a]
select query = Hasql.Statement bytes params decode prepare
  where
    bytes = encodeUtf8 (Text.pack sql)
    params = Hasql.noParams
    decode = Hasql.rowList (parse @exprs @a)
    prepare = False
    sql = show doc
    doc = ppSelect query


ppSelect :: Table Expr a => Query a -> Doc
ppSelect query =
  Opaleye.ppSql $ primSelectWith names (toCols exprs') primQuery'
  where
    names = namesFromLabels
    (exprs, primQuery, _) =
      Opaleye.runSimpleQueryArrStart (toOpaleye query) ()
    (exprs', primQuery') = case optimize primQuery of
      Empty -> (undefined, Opaleye.Product (pure (pure Opaleye.Unit)) never)
      Unit -> (exprs, Opaleye.Unit)
      Optimized pq -> (exprs, pq)
    never = pure (toPrimExpr false)


ppRows :: Table Expr a => Query a -> Doc
ppRows query = case optimize primQuery of
  -- Special case VALUES because we can't use DEFAULT inside a SELECT
  Optimized (Opaleye.Product ((_, Opaleye.Values symbols rows) :| []) [])
    | eqSymbols symbols (toList (T.exprs a)) ->
        Opaleye.ppValues_ (map Opaleye.sqlExpr <$> toList rows)
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


ppPrimSelect :: Query a -> (Optimized Doc, a)
ppPrimSelect query =
  (Opaleye.ppSql . primSelect <$> optimize primQuery, a)
  where
    (a, primQuery, _) = Opaleye.runSimpleQueryArrStart (toOpaleye query) ()


type Optimized :: Type -> Type
data Optimized a = Empty | Unit | Optimized a
  deriving stock (Functor, Foldable, Traversable)


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
