{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Table.Null
  ( NullTable(..)
  , nullableTable, nullTable, nullifyTable, unsafeUnnullifyTable
  , isNullTable, isNonNullTable
  , nameNullTable
  , toMaybeTable, toNullTable
  )
where

-- base
import Data.Kind ( Type )
import Prelude hiding ( null, undefined )

-- comonad
import Control.Comonad ( extract )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( not_ )
import Rel8.Kind.Context ( Reifiable )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
import Rel8.Table
  ( Table, Columns, Context, fromColumns, toColumns
  , FromExprs, fromResult, toResult
  , Transpose
  )
import Rel8.Table.Alternative
  ( AltTable, (<|>:)
  , AlternativeTable, emptyTable
  )
import Rel8.Table.Bool ( bool )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.Maybe ( MaybeTable, justTable, maybeTable, nothingTable )
import Rel8.Table.Nullify ( Nullify, isNull )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Projection ( Projectable, project )
import Rel8.Table.Serialize ( ToExprs )
import Rel8.Table.Undefined ( undefined )


-- | @NullTable t@ is the table @t@, but where all the columns in @t@ have the
-- possibility of being 'Rel8.null'. This is very similar to
-- 'Rel8.MaybeTable', except that it does not use an extra tag field, so it
-- cannot distinguish between @Nothing@ and @Just Nothing@ if nested. In other
-- words, if all of the columns of the @t@ passed to @NullTable@ are already
-- nullable, then @NullTable@ has no effect.
type NullTable :: K.Context -> Type -> Type
newtype NullTable context a = NullTable (Nullify context a)


instance Projectable (NullTable context) where
  project f (NullTable a) = NullTable (project f a)


instance context ~ Expr => AltTable (NullTable context) where
  ma <|>: mb = bool ma mb (isNullTable ma)


instance context ~ Expr => AlternativeTable (NullTable context) where
  emptyTable = nullTable


instance (Table context a, Reifiable context, context ~ context') =>
  Table context' (NullTable context a)
 where
  type Columns (NullTable context a) = Columns (Nullify context a)
  type Context (NullTable context a) = Context (Nullify context a)
  type FromExprs (NullTable context a) = FromExprs (Nullify context a)
  type Transpose to (NullTable context a) = NullTable to (Transpose to a)

  toColumns (NullTable a) = toColumns a
  fromColumns = NullTable . fromColumns

  toResult = toResult @_ @(Nullify context a)
  fromResult = fromResult @_ @(Nullify context a)


instance (EqTable a, context ~ Expr) => EqTable (NullTable context a) where
  eqTable = eqTable @(Nullify context a)


instance (OrdTable a, context ~ Expr) => OrdTable (NullTable context a) where
  ordTable = ordTable @(Nullify context a)


instance (ToExprs exprs a, context ~ Expr) =>
  ToExprs (NullTable context exprs) (Maybe a)


-- | Check if any of the non-nullable fields of @a@ are 'Rel8.null' under the
-- 'NullTable'. Returns 'Rel8.false' if @a@ has no non-nullable fields.
isNullTable :: Table Expr a => NullTable Expr a -> Expr Bool
isNullTable (NullTable a) = isNull a


-- | The inverse of 'isNullTable'.
isNonNullTable :: Table Expr a => NullTable Expr a -> Expr Bool
isNonNullTable = not_ . isNullTable


-- | Like 'Rel8.nullable'.
nullableTable :: (Table Expr a, Table Expr b)
  => b -> (a -> b) -> NullTable Expr a -> b
nullableTable b f ma@(NullTable a) = bool (f (extract a)) b (isNullTable ma)


-- | The null table. Like 'Rel8.null'.
nullTable :: Table Expr a => NullTable Expr a
nullTable = NullTable (pure undefined)


-- | Lift any table into 'NullTable'. Like 'Rel8.nullify'.
nullifyTable :: a -> NullTable Expr a
nullifyTable = NullTable . pure


unsafeUnnullifyTable :: NullTable Expr a -> a
unsafeUnnullifyTable (NullTable a) = extract a


-- | Construct a 'NullTable' in the 'Name' context. This can be useful if you
-- have a 'NullTable' that you are storing in a table and need to construct a
-- 'TableSchema'.
nameNullTable :: a -> NullTable Name a
nameNullTable = NullTable . pure


-- | Convert a 'NullTable' to a 'MaybeTable'.
toMaybeTable :: Table Expr a => NullTable Expr a -> MaybeTable Expr a
toMaybeTable = nullableTable nothingTable justTable


-- | Convert a 'MaybeTable' to a 'NullTable'. Note that if the underlying @a@
-- has no non-nullable fields, this is a lossy conversion.
toNullTable :: Table Expr a => MaybeTable Expr a -> NullTable Expr a
toNullTable = maybeTable nullTable nullifyTable
