{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Schema.Name
  ( Name(..)
  , Col( NameCol )
  , Selects
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Constraint, Type )
import Data.String ( IsString )
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Opaque ( Opaque )
import Rel8.Schema.Context ( Interpretation, Col )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler )
import Rel8.Schema.HTable.Type ( HType( HType ) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Nullability ( Sql )
import Rel8.Table ( Table, Columns, Context, fromColumns, toColumns )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Type ( DBType )


-- | A @Name@ is the name of a column, as it would be defined in a table's
-- schema definition. You can construct names by using the @OverloadedStrings@
-- extension and writing string literals. This is typically done when providing
-- a 'TableSchema' value.
type Name :: K.Context
newtype Name a = Name String
  deriving stock Show
  deriving newtype (IsString, Monoid, Semigroup)


instance Sql DBType a => Table Name (Name a) where
  type Columns (Name a) = HType a
  type Context (Name a) = Name

  toColumns (Name a) = HType (NameCol a)
  fromColumns (HType (NameCol a)) = Name a


instance Sql DBType a => Recontextualize Expr Name (Expr a) (Name a)


instance Sql DBType a => Recontextualize Identity Name (Identity a) (Name a)


instance Sql DBType a => Recontextualize Name Expr (Name a) (Expr a)


instance Sql DBType a => Recontextualize Name Identity (Name a) (Identity a)


instance Sql DBType a => Recontextualize Name Name (Name a) (Name a)


instance Interpretation Name where
  newtype Col Name _spec = NameCol String


instance Labelable Name where
  labeler (NameCol a) = NameCol a
  unlabeler (NameCol a) = NameCol a


-- | @Selects a b@ means that @a@ is a schema (i.e., a 'Table' of 'Name's) for
-- the 'Expr' columns in @b@.
type Selects :: Type -> Type -> Constraint
class Recontextualize Name Expr names exprs => Selects names exprs
instance Recontextualize Name Expr names exprs => Selects names exprs
instance {-# OVERLAPPING #-} Selects (Opaque Name Opaque) (Opaque Expr Opaque)
