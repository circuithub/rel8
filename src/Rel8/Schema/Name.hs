{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language StandaloneDeriving #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Schema.Name
  ( Name(..)
  , Col( N, unN )
  , Selects
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint, Type )
import Data.String ( IsString, fromString )
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Schema.Context ( Interpretation, Col )
import Rel8.Schema.HTable.Identity ( HIdentity( HType ), HType )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Reify ( notReify )
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Spec ( Spec( Spec ) )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns, reify, unreify
  )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Type ( DBType )


-- | A @Name@ is the name of a column, as it would be defined in a table's
-- schema definition. You can construct names by using the @OverloadedStrings@
-- extension and writing string literals. This is typically done when providing
-- a 'TableSchema' value.
type Name :: Type -> Type
data Name a where
  Name :: !String -> Name a


deriving stock instance Show (Name a)


instance IsString (Name a) where
  fromString = Name


instance Sql DBType a => Table Name (Name a) where
  type Columns (Name a) = HType a
  type Context (Name a) = Name

  toColumns a = HType (N a)
  fromColumns (HType (N a)) = a
  reify = notReify
  unreify = notReify


instance Sql DBType a => Recontextualize Expr Name (Expr a) (Name a)


instance Sql DBType a => Recontextualize Result Name (Identity a) (Name a)


instance Sql DBType a => Recontextualize Name Expr (Name a) (Expr a)


instance Sql DBType a => Recontextualize Name Result (Name a) (Identity a)


instance Sql DBType a => Recontextualize Name Name (Name a) (Name a)


instance Interpretation Name where
  data Col Name _spec where
    N :: {unN :: !(Name a)} -> Col Name ('Spec a)


-- | @Selects a b@ means that @a@ is a schema (i.e., a 'Table' of 'Name's) for
-- the 'Expr' columns in @b@.
type Selects :: Type -> Type -> Constraint
class Recontextualize Name Expr names exprs => Selects names exprs
instance Recontextualize Name Expr names exprs => Selects names exprs
