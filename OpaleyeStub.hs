{-# LANGUAGE RankNTypes #-}
module OpaleyeStub where

import Control.Arrow
import Control.Category
import Data.Profunctor
import Data.Profunctor.Product
import Data.Int

-- import qualified Opaleye.Internal.Column as O
-- import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
-- import qualified Opaleye.Internal.Join as O
-- import qualified Opaleye.Internal.PGTypes as O
-- import qualified Opaleye.Internal.PackMap as O
-- import qualified Opaleye.Internal.QueryArr as O
-- import qualified Opaleye.Internal.RunQuery as O
-- import qualified Opaleye.Internal.Table as O
-- import qualified Opaleye.Internal.TableMaker as O
-- import qualified Opaleye.Internal.Unpackspec as O
-- import qualified Opaleye.Join as O
-- import qualified Opaleye.RunQuery as O
-- import qualified Opaleye.Table as O

data PackMap a b s t =
  PackMap (forall f. Applicative f =>
                       (a -> f b) -> s -> f t)

newtype Unpackspec columns columns' =
  Unpackspec (PackMap PrimExpr PrimExpr columns columns')

instance Functor (Unpackspec columns)
instance Profunctor Unpackspec
instance ProductProfunctor Unpackspec

data Literal
  = BoolLit Bool
  | IntegerLit Integer
  | NullLit

data PrimExpr
  = ConstExpr Literal
  | BaseTableAttrExpr String
  | BinExpr Operator PrimExpr PrimExpr
  | UnExpr UnOp PrimExpr

data UnOp = OpNot

data Operator
  = (:==)
  | OpAnd
  | OpOr

newtype Column pgType = Column PrimExpr

newtype NullMaker a b = NullMaker (a -> b)

data QueryArr a b

instance Functor (QueryArr a)
instance Applicative (QueryArr a)
instance Arrow QueryArr
instance Category QueryArr

type Query = QueryArr ()

data QueryRunner columns haskells =
  QueryRunner (Unpackspec columns ())
              (columns -> RowParser haskells)
              (columns -> Bool)

data RowParser a

instance Functor RowParser
instance Applicative RowParser
instance Monad RowParser

data TableProperties writerColumns viewColumns = TableProperties
   { tablePropertiesWriter :: Writer writerColumns viewColumns
   , tablePropertiesView   :: View viewColumns }

data View columns = View columns

newtype Writer columns dummy =
  Writer (forall f. Functor f =>
          PackMap (f PrimExpr, String) () (f columns) ())

data Table writerColumns viewColumns
  = Table String (TableProperties writerColumns viewColumns)
  | TableWithSchema String String (TableProperties writerColumns viewColumns)


queryTableExplicit :: ColumnMaker tablecolumns columns
                   -> Table a tablecolumns
                   -> Query columns
queryTableExplicit = undefined

newtype ColumnMaker columns columns' =
  ColumnMaker (PackMap PrimExpr PrimExpr columns columns')

runQueryExplicit :: QueryRunner columns haskells
                 -> Connection
                 -> Query columns
                 -> IO [haskells]
runQueryExplicit = undefined

data Connection

leftJoinExplicit :: Unpackspec columnsL columnsL
                 -> Unpackspec columnsR columnsR
                 -> NullMaker columnsR nullableColumnsR
                 -> Query columnsL -> Query columnsR
                 -> ((columnsL, columnsR) -> Column PGBool)
                 -> Query (columnsL, nullableColumnsR)
leftJoinExplicit = undefined

data PGBool

class FromField a
instance FromField Bool
instance FromField Int
instance FromField Int64
instance FromField a => FromField (Maybe a)

field :: FromField a => RowParser a
field = undefined

aggregate :: Aggregator a b -> Query a -> Query b
aggregate = undefined

newtype Aggregator a b =
  Aggregator (PackMap (Maybe (AggrOp, [OrderExpr]), PrimExpr) PrimExpr a b)

instance Profunctor Aggregator
instance ProductProfunctor Aggregator

data AggrOp
  = AggrCount
  | AggrSum
  | AggrAvg
  | AggrMin
  | AggrMax
  | AggrStdDev
  | AggrStdDevP
  | AggrVar
  | AggrVarP
  | AggrBoolOr
  | AggrBoolAnd
  | AggrArr
  | AggrStringAggr PrimExpr
  | AggrOther String

data OrderExpr =
  OrderExpr OrderOp
            PrimExpr

data OrderNulls
  = NullsFirst
  | NullsLast

data OrderDirection
  = OpAsc
  | OpDesc

data OrderOp = OrderOp
  { orderDirection :: OrderDirection
  , orderNulls :: OrderNulls
  }
