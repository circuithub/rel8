{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Table
  ( Table
      ( Columns, Context, fromColumns, toColumns
      , FromExprs, fromResult, toResult
      )
  , Congruent
  , TTable, TColumns, TContext, TFromExprs
  )
where

-- base
import Data.Kind ( Constraint, Type )
import GHC.Generics ( Generic, Rep, from, to )
import Prelude hiding ( null )

-- rel8
import Rel8.FCF ( Eval, Exp )
import Rel8.Generic.Map ( GMap )
import Rel8.Generic.Table.Record
  ( GTable, GColumns, GContext
  , gfromColumns, gtoColumns, gfromResult, gtoResult
  )
import Rel8.Generic.Record ( Record(..) )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Identity ( HIdentity(..) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Spec ( KnownSpec )


-- | @Table@s are one of the foundational elements of Rel8, and describe data
-- types that have a finite number of columns. Each of these columns contains
-- data under a shared context, and contexts describe how to interpret the
-- metadata about a column to a particular Haskell type. In Rel8, we have
-- contexts for expressions (the 'Rel8.Expr' context), aggregations (the
-- 'Rel8.Aggregate' context), insert values (the 'Rel8.Insert' contex), among
-- others.
--
-- In typical usage of Rel8 you don't need to derive instances of 'Table'
-- yourself, as anything that's an instance of 'Rel8.Rel8able' is always a
-- 'Table'.
type Table :: K.HContext -> Type -> Constraint
class (HTable (Columns a), context ~ Context a) => Table context a | a -> context where
  -- | The 'HTable' functor that describes the schema of this table.
  type Columns a :: K.HTable

  -- | The common context that all columns use as an interpretation.
  type Context a :: K.HContext

  -- | The @FromExprs@ type family maps a type in the @Expr@ context to the
  -- corresponding Haskell type.
  type FromExprs a :: Type

  toColumns :: a -> Columns a context
  fromColumns :: Columns a context -> a

  fromResult :: Columns a Result -> FromExprs a
  toResult :: FromExprs a -> Columns a Result

  type Columns a = GColumns TColumns (Rep (Record a))
  type Context a = GContext TContext (Rep (Record a))
  type FromExprs a = DefaultFromExprs a

  default toColumns ::
    ( Generic (Record a)
    , GTable (TTable context) TColumns TFromExprs (Rep (Record a))
    , Columns a ~ GColumns TColumns (Rep (Record a))
    )
    => a -> Columns a context
  toColumns =
    gtoColumns
      @(TTable context)
      @TColumns
      @TFromExprs
      toColumns .
    from .
    Record

  default fromColumns ::
    ( Generic (Record a)
    , GTable (TTable context) TColumns TFromExprs (Rep (Record a))
    , Columns a ~ GColumns TColumns (Rep (Record a))
    )
    => Columns a context -> a
  fromColumns =
    unrecord .
    to .
    gfromColumns
      @(TTable context)
      @TColumns
      @TFromExprs
      fromColumns

  default toResult ::
    ( Generic (Record (FromExprs a))
    , GTable (TTable context) TColumns TFromExprs (Rep (Record a))
    , Columns a ~ GColumns TColumns (Rep (Record a))
    , Rep (Record (FromExprs a)) ~ GMap TFromExprs (Rep (Record a))
    )
    => FromExprs a -> Columns a Result
  toResult =
    gtoResult
      @(TTable context)
      @TColumns
      @TFromExprs
      @(Rep (Record a))
      (\(_ :: proxy x) -> toResult @(Context x) @x) .
    from .
    Record

  default fromResult ::
    ( Generic (Record (FromExprs a))
    , GTable (TTable context) TColumns TFromExprs (Rep (Record a))
    , Columns a ~ GColumns TColumns (Rep (Record a))
    , Rep (Record (FromExprs a)) ~ GMap TFromExprs (Rep (Record a))
    )
    => Columns a Result -> FromExprs a
  fromResult =
    unrecord .
    to .
    gfromResult
      @(TTable context)
      @TColumns
      @TFromExprs
      @(Rep (Record a))
      (\(_ :: proxy x) -> fromResult @(Context x) @x)


type DefaultFromExprs :: Type -> Type
type family DefaultFromExprs a where
  DefaultFromExprs (t a b c d e f g) =
    t (FromExprs a) (FromExprs b) (FromExprs c) (FromExprs d) (FromExprs e)
      (FromExprs f) (FromExprs g)
  DefaultFromExprs (t a b c d e f) =
    t (FromExprs a) (FromExprs b) (FromExprs c) (FromExprs d) (FromExprs e)
      (FromExprs f)
  DefaultFromExprs (t a b c d e) =
    t (FromExprs a) (FromExprs b) (FromExprs c) (FromExprs d) (FromExprs e)
  DefaultFromExprs (t a b c d) =
    t (FromExprs a) (FromExprs b) (FromExprs c) (FromExprs d)
  DefaultFromExprs (t a b c) = t (FromExprs a) (FromExprs b) (FromExprs c)
  DefaultFromExprs (t a b) = t (FromExprs a) (FromExprs b)
  DefaultFromExprs (t a) = t (FromExprs a)


data TTable :: K.HContext -> Type -> Exp Constraint
type instance Eval (TTable context a) = Table context a


data TColumns :: Type -> Exp K.HTable
type instance Eval (TColumns a) = Columns a


data TContext :: Type -> Exp K.HContext
type instance Eval (TContext a) = Context a


data TFromExprs :: Type -> Exp Type
type instance Eval (TFromExprs a) = FromExprs a


-- | Any context is trivially a table.
instance KnownSpec spec => Table context (context spec) where
  type Columns (context spec) = HIdentity spec
  type Context (context spec) = context
  type FromExprs (context spec) = Result spec

  toColumns = HIdentity
  fromColumns = unHIdentity
  toResult = HIdentity
  fromResult = unHIdentity


instance (Table context a, Table context b) => Table context (a, b)


instance
  ( Table context a, Table context b, Table context c
  )
  => Table context (a, b, c)


instance
  ( Table context a, Table context b, Table context c, Table context d
  )
  => Table context (a, b, c, d)


instance
  ( Table context a, Table context b, Table context c, Table context d
  , Table context e
  )
  => Table context (a, b, c, d, e)


instance
  ( Table context a, Table context b, Table context c, Table context d
  , Table context e, Table context f
  )
  => Table context (a, b, c, d, e, f)


instance
  ( Table context a, Table context b, Table context c, Table context d
  , Table context e, Table context f, Table context g
  )
  => Table context (a, b, c, d, e, f, g)


type Congruent :: Type -> Type -> Constraint
class Columns a ~ Columns b => Congruent a b
instance Columns a ~ Columns b => Congruent a b
