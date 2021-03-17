{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}

module Rel8.DBType.DBSum ( DBSum(..) ) where

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


-- | The class of data types that can be aggregated under the @sum@ operation.
-- This type class contains two parameters, as @sum@ can be a type-changing
-- operation in PostgreSQL.
class DBType a => DBSum a res | a -> res where
  -- | Corresponds to @sum@.
  sum :: Expr a -> Aggregate (Expr res)
  sum (Expr a) = Aggregate $ Expr $ Opaleye.AggrExpr Opaleye.AggrAll Opaleye.AggrSum a []


instance DBSum Double Double


instance DBSum Float Float


instance DBSum Int16 Int64


instance DBSum Int32 Int64


instance DBSum Int64 Scientific


instance DBSum Scientific Scientific
