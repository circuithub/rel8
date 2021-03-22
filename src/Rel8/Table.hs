{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Table ( Table(..), AllColumns, nullTable ) where

-- base
import Data.Kind ( Constraint, Type )

-- rel8
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Context ( Column, Defaulting( NoDefault ), Meta( Meta ) )
import Rel8.Expr ( Expr )
import Rel8.Expr.Instances ( Column( ExprColumn, fromExprColumn ) )
import Rel8.Expr.Opaleye ( fromPrimExpr )
import Rel8.HTable ( ColType, HAllColumns, HTable, htabulateMeta )
import Rel8.HTable.HIdentity ( HIdentity( HIdentity, unHIdentity ) )
import Rel8.HTable.HPair ( HPair( HPair ) )
import Rel8.Info ( HasInfo )


-- | Types that represent SQL tables.
-- 
-- You generally should not need to derive instances of this class manually, as
-- writing higher-kinded data types is usually more convenient. See also:
-- 'HigherKindedTable'.
class HTable (Columns t) => Table (context :: Type -> Type) (t :: Type) | t -> context where
  type Columns t :: (Meta -> Type) -> Type

  toColumns :: t -> Columns t (Column context)
  fromColumns :: Columns t (Column context) -> t


type AllColumns (t :: Type) (c :: Type -> Constraint) = HAllColumns (Columns t) (ColType c)


instance (HasInfo a, expr ~ Expr) => Table expr (Expr a) where
  type Columns (Expr a) = HIdentity ('Meta 'NoDefault a)

  toColumns = HIdentity . ExprColumn
  fromColumns = fromExprColumn . unHIdentity


instance (HTable t, Column f ~ g) => Table f (t g) where
  type Columns (t g) = t

  toColumns = id
  fromColumns = id


instance (Table f a, Table f b) => Table f (a, b) where
  type Columns (a, b) = HPair (Columns a) (Columns b)

  toColumns (a, b) = HPair (toColumns a) (toColumns b)
  fromColumns (HPair x y) = (fromColumns x, fromColumns y)


instance (Table f a, Table f b, Table f c) => Table f (a, b, c) where
  type Columns (a, b, c) = HPair (Columns a) (HPair (Columns b) (Columns c))

  toColumns (a, b, c) = HPair (toColumns a) $ HPair (toColumns b) (toColumns c)
  fromColumns (HPair x (HPair y z)) = (fromColumns x, fromColumns y, fromColumns z)


instance (Table f a, Table f b, Table f c, Table f d) => Table f (a, b, c, d) where
  type Columns (a, b, c, d) = HPair (HPair (Columns a) (Columns b)) (HPair (Columns c) (Columns d))

  toColumns (a, b, c, d) = HPair (HPair (toColumns a) (toColumns b)) (HPair (toColumns c) (toColumns d))
  fromColumns (HPair (HPair a b) (HPair c d)) = (fromColumns a, fromColumns b, fromColumns c, fromColumns d)


nullTable :: Table Expr a => a
nullTable =
  fromColumns $
    htabulateMeta \_ ->
      ExprColumn $ fromPrimExpr $ Opaleye.ConstExpr Opaleye.NullLit
