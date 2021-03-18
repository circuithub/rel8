module Rel8.DBType.DBMax ( DBMax(..) ) where

-- base
import Data.Int ( Int16, Int32, Int64 )

-- rel8
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Aggregate ( Aggregate, aggregateAllExprs )
import Rel8.DBType ( DBType )
import Rel8.Expr ( Expr )

-- scientific
import Data.Scientific ( Scientific )

-- text
import Data.Text ( Text )

-- time
import Data.Time ( UTCTime )


-- | The class of 'DBType's that support the @max@ aggregation function.
--
-- If you have a custom type that you know supports @max@, you can use
-- @DeriveAnyClass@ to derive a default implementation that calls @max@.
class DBType a => DBMax a where
  -- | Produce an aggregation for @Expr a@ using the @max@ function.
  max :: Expr a -> Aggregate (Expr a)
  max = aggregateAllExprs Opaleye.AggrMax


instance DBMax Int64


instance DBMax Double


instance DBMax Int16


instance DBMax Int32


instance DBMax Scientific


instance DBMax Float


instance DBMax Text


instance DBMax UTCTime


instance DBMax a => DBMax (Maybe a)
