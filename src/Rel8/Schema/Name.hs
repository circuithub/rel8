{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rel8.Schema.Name (
  Name (..),
  Selects,
  ppColumn,
)
where

-- base
import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Constraint, Type)
import Data.String (IsString)
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.Sql as Opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye

-- pretty
import Text.PrettyPrint (Doc)

-- rel8
import Rel8.Expr (Expr)
import Rel8.Schema.HTable.Identity (HIdentity (HIdentity))
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Null (Sql)
import Rel8.Table (
  Columns,
  Context,
  FromExprs,
  Table,
  Transpose,
  fromColumns,
  fromResult,
  toColumns,
  toResult,
 )
import Rel8.Table.Transpose (Transposes)
import Rel8.Type (DBType)


{- | A @Name@ is the name of a column, as it would be defined in a table's
schema definition. You can construct names by using the @OverloadedStrings@
extension and writing string literals. This is typically done when providing
a 'TableSchema' value.
-}
type Name :: K.Context
newtype Name a = Name String
  deriving stock (Show)
  deriving newtype (IsString)


instance Sql DBType a => Table Name (Name a) where
  type Columns (Name a) = HIdentity a
  type Context (Name a) = Name
  type FromExprs (Name a) = a
  type Transpose to (Name a) = to a


  toColumns a = HIdentity a
  fromColumns (HIdentity a) = a
  toResult a = HIdentity (Identity a)
  fromResult (HIdentity (Identity a)) = a


{- | @Selects a b@ means that @a@ is a schema (i.e., a 'Table' of 'Name's) for
the 'Expr' columns in @b@.
-}
type Selects :: Type -> Type -> Constraint
class Transposes Name Expr names exprs => Selects names exprs


instance Transposes Name Expr names exprs => Selects names exprs


ppColumn :: String -> Doc
ppColumn = Opaleye.ppSqlExpr . Opaleye.ColumnSqlExpr . Opaleye.SqlColumn
