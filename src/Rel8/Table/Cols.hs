{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Cols
  ( Cols( Cols )
  , fromCols
  , toCols
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- rel8
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.Result ( Result )
import Rel8.Table ( Table(..) )
import Rel8.Table.Recontextualize ( Recontextualize )


type Cols :: K.Context -> K.HTable -> Type
newtype Cols context columns = Cols (columns context)


instance (HTable columns, context ~ context') =>
  Table context' (Cols context columns)
 where
  type Columns (Cols context columns) = columns
  type Context (Cols context columns) = context
  type FromExprs (Cols context columns) = Cols Result columns

  toColumns (Cols a) = a
  fromColumns = Cols

  toResult (Cols a) = a
  fromResult = Cols


instance HTable t => Recontextualize from to (Cols from t) (Cols to t)


fromCols :: Table context a => Cols context (Columns a) -> a
fromCols (Cols a) = fromColumns a


toCols :: Table context a => a -> Cols context (Columns a)
toCols = Cols . toColumns
