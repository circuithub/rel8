{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}

module Rel8.Expr.Subscript
  ( unsafeSubscript
  , unsafeSubscripts
  )
where

-- base
import Data.Foldable (foldl')
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Opaleye (fromPrimExpr, toPrimExpr)
import Rel8.Schema.HTable (hfoldMap)
import Rel8.Schema.Null (Sql, Unnullify)
import Rel8.Table (Table, toColumns)
import Rel8.Type (DBType, typeInformation)
import Rel8.Type.Array (extractArrayElement)
import Rel8.Type.Information (TypeInformation)


-- | @'unsafeSubscript' a i@ will generate the SQL @a[i]@.
--
-- Note that this function is not type checked and the generated SQL has no
-- casts. This is only intended an escape hatch to be used if Rel8 cannot
-- otherwise express the expression you need. If you find yourself using this
-- function, please let us know, as it may indicate that something is missing
-- from Rel8!
unsafeSubscript :: Sql DBType b => Expr a -> Expr i -> Expr b
unsafeSubscript = sunsafeSubscript typeInformation


-- | @'unsafeSubscripts' a (i, j)@ will generate the SQL @a[i][j]@.
--
-- Note that this function is not type checked and the generated SQL has no
-- casts. This is only intended an escape hatch to be used if Rel8 cannot
-- otherwise express the expression you need. If you find yourself using this
-- function, please let us know, as it may indicate that something is missing
-- from Rel8!
unsafeSubscripts :: (Table Expr i, Sql DBType b) => Expr a -> i -> Expr b
unsafeSubscripts = sunsafeSubscripts typeInformation


sunsafeSubscript :: TypeInformation (Unnullify b) -> Expr a -> Expr i -> Expr b
sunsafeSubscript info array i =
  fromPrimExpr . extractArrayElement info $
    Opaleye.ArrayIndex (toPrimExpr array) (toPrimExpr i)


sunsafeSubscripts :: Table Expr i => TypeInformation (Unnullify b) -> Expr a -> i -> Expr b
sunsafeSubscripts info array i =
  fromPrimExpr $ extractArrayElement info $ primSubscripts array indices
  where
    indices = hfoldMap (pure . toPrimExpr) $ toColumns i


primSubscripts :: Expr a -> [Opaleye.PrimExpr] -> Opaleye.PrimExpr
primSubscripts array indices =
  foldl' Opaleye.ArrayIndex (toPrimExpr array) indices
