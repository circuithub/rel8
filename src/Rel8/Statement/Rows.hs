{-# language DataKinds #-}
{-# language GADTs #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Statement.Rows
  ( Rows (..)    
  )
where

-- base
import Data.Int (Int64)
import Data.Kind (Type)
import Prelude

-- rel8
import Rel8.Table.Serialize (Serializable)

-- vector
import Data.Vector (Vector)


-- | To 'Rel8.run' a statement, a 'Rows' parameter is required. 'Rows'
-- specifies how the rows (if any) returned by the given statement should be
-- processed in Haskell.
type Rows :: Maybe Type -> Type -> Type
data Rows returning result where
  Void :: Rows returning ()
  -- ^ 'Void' means you don't care about the rows returned by the statement.
  -- A statement 'Rel8.run' with 'Void' will always return @()@ whether the
  -- statement itself produces any rows or not.
  RowsAffected :: Rows 'Nothing Int64
  -- ^ 'RowsAffected' returns the number of rows that were affected by an
  -- @INSERT@, @UPDATE@ or @DELETE@ statement. The statement must not be a
  -- 'Rel8.Query' and must have a @returning@ field set to
  -- 'Rel8.NoReturning'.
  Single :: Serializable exprs a => Rows ('Just exprs) a
  -- ^ 'Single' decodes a single row of @exprs@ as an @a@. If the
  -- corresponding statement returns a number of rows other than 1, then a
  -- runtime exception is thrown. The statement must be either a 'Rel8.Query'
  -- or have a @returning@ field set to 'Rel8.Returning'.
  Maybe :: Serializable exprs a => Rows ('Just exprs) (Maybe a)
  -- ^ 'Maybe' decodes zero or one rows of @exprs@ into a @Maybe a@. If the
  -- corresponding statement returns a number of rows other than 0 or 1, then
  -- a runtime exception is thrown. The statement must be either a 'Rel8.Query'
  -- or have a @returning@ field set to 'Rel8.Returning'. 
  List :: Serializable exprs a => Rows ('Just exprs) [a]
  -- ^ 'List' decodes any number of rows of @exprs@ into a @[a]@. The
  -- statement must be either a 'Rel8.Query' or have a @returning@ field set
  -- to 'Rel8.Returning'.
  Vector :: Serializable exprs a => Rows ('Just exprs) (Vector a)
  -- ^ 'List' decodes any number of rows of @exprs@ into a @Vector a@. The
  -- statement must be either a 'Rel8.Query' or have a @returning@ field set
  -- to 'Rel8.Returning'.