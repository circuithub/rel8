{-# language DeriveTraversable #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Statement.Select
  ( select
  , ppSelect

  , Optimized(..)
  , ppPrimSelect
  )
where

-- base
import Control.Exception ( throwIO )
import Data.Void ( Void )
import Prelude

-- hasql
import Hasql.Connection ( Connection )
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.Sql as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.Print as Opaleye
import qualified Opaleye.Internal.Optimize as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye hiding ( Select )
import qualified Opaleye.Internal.Sql as Opaleye

-- pretty
import Text.PrettyPrint ( Doc )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Query ( Query )
import Rel8.Query.Opaleye ( toOpaleye )
import Rel8.Schema.Name ( Selects )
import Rel8.Table ( Table )
import Rel8.Table.Cols ( toCols )
import Rel8.Table.Name ( namesFromLabels )
import Rel8.Table.Opaleye ( castTable, exprsWithNames )
import Rel8.Table.Serialize ( Serializable, parse )

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )


-- | Run a @SELECT@ statement, returning all rows.
select :: forall exprs a. Serializable exprs a
  => Connection -> Query exprs -> IO [a]
select c query = case ppSelect query of
  Nothing -> pure []
  Just doc -> Hasql.run session c >>= either throwIO pure
    where
      session = Hasql.statement () statement
      statement = Hasql.Statement bytes params decode prepare
      bytes = encodeUtf8 (Text.pack sql)
      params = Hasql.noParams
      decode = Hasql.rowList (parse @exprs @a)
      prepare = False
      sql = show doc


ppSelect :: Table Expr a => Query a -> Maybe Doc
ppSelect query = do
  primQuery' <- case optimize primQuery of
    Empty -> Nothing
    Unit -> Just Opaleye.Unit
    Optimized primQuery' -> Just primQuery'
  pure $ Opaleye.ppSql $ primSelectWith names (toCols exprs) primQuery'
  where
    names = namesFromLabels
    (exprs, primQuery, _) =
      Opaleye.runSimpleQueryArrStart (toOpaleye query) ()


ppPrimSelect :: Query a -> (Optimized Doc, a)
ppPrimSelect query =
  (Opaleye.ppSql . primSelect <$> optimize primQuery, a)
  where
    (a, primQuery, _) =
      Opaleye.runSimpleQueryArrStart (toOpaleye query) ()


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
