module Rel8.DBType.DBMax ( DBMax(..) ) where

-- base
import Data.Int ( Int16, Int32, Int64 )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Aggregate ( Aggregate( Aggregate ) )
import Rel8.DBType ( DBType )
import Rel8.Expr ( Expr( Expr ) )

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
  max (Expr a) = Aggregate $ Expr $ Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrMax a []


instance DBMax Int64


instance DBMax Double


instance DBMax Int16


instance DBMax Int32


instance DBMax Scientific


instance DBMax Float


instance DBMax Text


instance DBMax UTCTime


instance DBMax a => DBMax (Maybe a)
