{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table.AsRel8able
  ( AsRel8able(..)
  , toCols, fromCols
  ) where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.Result ( Result )
import Rel8.Table ( Table(..) )
import Rel8.Table.Recontextualize


-- | Interpret any 'HTable' as a 'Rel8able'.
type AsRel8able :: K.Context -> K.HTable -> Type
newtype AsRel8able f htable = AsRel8able { toHTable :: htable f }


instance (f ~ g, HTable t) => Table f (AsRel8able g t) where
  type Columns (AsRel8able g t) = t
  type Context (AsRel8able g t) = g
  type FromExprs (AsRel8able g t) = t Result

  toColumns = toHTable
  fromColumns = AsRel8able

  fromResult = id
  toResult = id


instance HTable t => Recontextualize from to (AsRel8able from t) (AsRel8able to t)


toCols :: Table f a => a -> AsRel8able f (Columns a)
toCols = AsRel8able . toColumns


fromCols :: Table f a => AsRel8able f (Columns a) -> a
fromCols = fromColumns . toHTable
