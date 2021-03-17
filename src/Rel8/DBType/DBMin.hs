module Rel8.DBType.DBMin ( DBMin(..) ) where

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


-- | The class of 'DBType's that support the @min@ aggregation function.
--
-- If you have a custom type that you know supports @min@, you can use
-- @DeriveAnyClass@ to derive a default implementation that calls @min@.
class DBType a => DBMin a where
  -- | Produce an aggregation for @Expr a@ using the @max@ function.
  min :: Expr a -> Aggregate (Expr a)
  min (Expr a) = Aggregate $ Expr $ Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrMin a []


instance DBMin Double


instance DBMin Float


instance DBMin Int16


instance DBMin Int32


instance DBMin Int64


instance DBMin Scientific


instance DBMin Text


instance DBMin UTCTime


instance DBMin a => DBMin (Maybe a) -- TODO: Do we want this?
