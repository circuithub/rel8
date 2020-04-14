module Rel8.Column where

import Control.Applicative ( Const(..) )
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye


newtype Column a =
  Column { toPrimExpr :: Opaleye.PrimExpr }


selectColumn :: Const String a -> Column a
selectColumn = Column . Opaleye.BaseTableAttrExpr . getConst


traversePrimExpr
  :: Functor f
  => (Opaleye.PrimExpr -> f Opaleye.PrimExpr) -> Column a -> f (Column a)
traversePrimExpr f (Column x) =
  Column <$> f x
