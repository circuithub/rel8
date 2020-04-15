module Rel8.Column
  ( Column(..)
  , case_
  , fromJust
  , just
  , lit
  , selectColumn
  , toOpaleyeColumn
  , traversePrimExpr
  , write
  , zipColumnsM
  ) where

import Control.Applicative ( Const(..) )
import Data.Coerce ( coerce )
import qualified Opaleye.Internal.Column as Opaleye
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


zipColumnsM
  :: Applicative m
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr -> m Opaleye.PrimExpr)
  -> Column a -> Column a -> m (Column a)
zipColumnsM f (Column x) (Column y) = fmap Column $ f x y


toOpaleyeColumn :: Column a -> Opaleye.Column b
toOpaleyeColumn = coerce


fromJust :: Column (Maybe a) -> Column a
fromJust = coerce


case_ :: [(Column Bool, Column a)] -> Column a -> Column a
case_ = coerce Opaleye.CaseExpr


lit :: Opaleye.Literal -> Column a
lit = coerce Opaleye.ConstExpr


write :: Functor f => ((f Opaleye.PrimExpr, String) -> m ()) -> f (Column a) -> Const String a -> m ()
write f values (Const columnName) = f (fmap toPrimExpr values, columnName)


just :: Column a -> Column (Maybe a)
just = coerce
