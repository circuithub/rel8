{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.Name
  ( Name(..)
  , Selects
  )
where

-- base
import Data.Functor.Identity ( Identity( Identity ) )
import Data.Kind ( Constraint, Type )
import Data.String ( IsString )
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.HTable.Identity ( HIdentity( HIdentity ) )
import Rel8.Schema.Null ( Sql )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , FromExprs, fromResult, toResult
  , Transpose
  )
import Rel8.Table.Transpose ( Transposes )
import Rel8.Type ( DBType )


-- | A @Name@ is the name of a column, as it would be defined in a table's
-- schema definition. You can construct names by using the @OverloadedStrings@
-- extension and writing string literals. This is typically done when providing
-- a 'TableSchema' value.
type Name :: K.Context
newtype Name a = Name String
  deriving stock Show
  deriving newtype IsString


instance Sql DBType a => Table Name (Name a) where
  type Columns (Name a) = HIdentity a
  type Context (Name a) = Name
  type FromExprs (Name a) = a
  type Transpose to (Name a) = to a

  toColumns a = HIdentity a
  fromColumns (HIdentity a) = a
  toResult a = HIdentity (Identity a)
  fromResult (HIdentity (Identity a)) = a


-- | @Selects a b@ means that @a@ is a schema (i.e., a 'Table' of 'Name's) for
-- the 'Expr' columns in @b@.
type Selects :: Type -> Type -> Constraint
class Transposes Name Expr names exprs => Selects names exprs
instance Transposes Name Expr names exprs => Selects names exprs
