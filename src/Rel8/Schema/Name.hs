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

module Rel8.Schema.Name
  ( Name(..)
  , Selects
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.String ( IsString, fromString )
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Schema.Context.Lower ( Lower )
import Rel8.Schema.HTable.Identity ( HIdentity( HType ), HType )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Result ( Result( R ) )
import Rel8.Schema.Spec ( Spec( Spec ) )
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
type Name :: k -> Type
data Name a where
  Name :: forall (a :: Type). !String -> Name a
  N :: { unN :: !(Name a) } -> Name ('Spec a)


type instance Lower Name = Name


deriving stock instance Show (Name a)


instance k ~ Type => IsString (Name (a :: k)) where
  fromString = Name


instance Sql DBType a => Table Name (Name a) where
  type Columns (Name a) = HType a
  type Context (Name a) = Name
  type FromExprs (Name a) = a
  type Transpose to (Name a) = Lower to a

  toColumns a = HType (N a)
  fromColumns (HType (N a)) = a
  toResult a = HType (R a)
  fromResult (HType (R a)) = a


-- | @Selects a b@ means that @a@ is a schema (i.e., a 'Table' of 'Name's) for
-- the 'Expr' columns in @b@.
type Selects :: Type -> Type -> Constraint
class Transposes Name Expr names exprs => Selects names exprs
instance Transposes Name Expr names exprs => Selects names exprs
