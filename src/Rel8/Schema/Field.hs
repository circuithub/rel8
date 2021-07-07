{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}

module Rel8.Schema.Field
  ( Field(..)
  , fields
  )
where

-- base
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Type )
import Prelude

-- rel8
import Rel8.Schema.HTable ( HField, htabulate )
import Rel8.Schema.HTable.Identity ( HIdentity( HIdentity ) )
import Rel8.Schema.Kind as K
import Rel8.Schema.Null ( Sql )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , FromExprs, fromResult, toResult
  , Transpose
  )
import Rel8.Table.Transpose ( Transposes )
import Rel8.Type ( DBType )


-- | A special context used in the construction of 'Rel8.Projection's.
type Field :: Type -> K.Context
newtype Field table a = Field (HField (Columns table) a)


instance Sql DBType a => Table (Field table) (Field table a) where
  type Columns (Field table a) = HIdentity a
  type Context (Field table a) = Field table
  type FromExprs (Field table a) = a
  type Transpose to (Field table a) = to a

  toColumns = HIdentity
  fromColumns (HIdentity a) = a
  toResult a = HIdentity (Identity a)
  fromResult (HIdentity (Identity a)) = a


fields :: Transposes context (Field table) table fields => fields
fields = fromColumns $ htabulate Field
