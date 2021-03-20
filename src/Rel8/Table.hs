{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table ( Table(..) ) where

-- base
import Data.Kind ( Type )

-- rel8
import Rel8.Context ( Context, KContext )
import Rel8.Expr ( Expr )
import Rel8.HTable ( HTable )
import Rel8.HTable.HIdentity ( HIdentity( HIdentity, unHIdentity ) )
import Rel8.HTable.HPair ( HPair( HPair ) )
import Rel8.Info ( HasInfo )


-- | Types that represent SQL tables.
-- 
-- You generally should not need to derive instances of this class manually, as
-- writing higher-kinded data types is usually more convenient. See also:
-- 'HigherKindedTable'.
class HTable (Columns t) => Table (context :: Type -> Type) (t :: Type) | t -> context where
  type Columns t :: KContext -> Type

  toColumns :: t -> Columns t (Context context)
  fromColumns :: Columns t (Context context) -> t


instance (HasInfo a, expr ~ Expr) => Table expr (Expr a) where
  type Columns (Expr a) = HIdentity a
  toColumns = HIdentity
  fromColumns = unHIdentity


instance (HTable t, f ~ g) => Table f (t (Context g)) where
  type Columns (t (Context g)) = t
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
