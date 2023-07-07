{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Rel8.Statement.Rows (
  Rows (..),
)
where

-- base
import Data.Int (Int64)
import Data.Kind (Type)
import Prelude

-- rel8
import Rel8.Query (Query)
import Rel8.Table.Serialize (Serializable)

-- vector
import Data.Vector (Vector)


type Rows :: Type -> Type -> Type
data Rows returning result where
  Void :: Rows returning ()
  RowsAffected :: Rows () Int64
  Single :: Serializable exprs a => Rows (Query exprs) a
  Maybe :: Serializable exprs a => Rows (Query exprs) (Maybe a)
  List :: Serializable exprs a => Rows (Query exprs) [a]
  Vector :: Serializable exprs a => Rows (Query exprs) (Vector a)
