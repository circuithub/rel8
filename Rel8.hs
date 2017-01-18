{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Rel8
  ( -- $intro

    -- * Defining Tables
    Column(..)
  , HasDefault(..)
  , Nullable(..)
  , C
  , BaseTable(..)

    -- * Tables
  , Table(..)
  , leftJoin
  , MaybeRow(..)
  , Col

    -- * Expressions
  , Expr

    -- * Literals
  , Lit(..)

    -- ** Operators
  , (^/=^)

    -- ** Null
  , toNullable
  , (?)

    -- * Querying Tables
  , select
  , QueryResult

    -- * Re-exported symbols
  , Connection, Stream, Of, Generic
  ) where

import Data.Tagged (Tagged(..))
import Control.Applicative ((<$), liftA2)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (fromMaybe)
import Data.Profunctor (dimap)
import Data.Profunctor.Product ((***!))
import Data.Proxy (Proxy(..))
-- import Database.PostgreSQL.Simple (Connection)
-- import Database.PostgreSQL.Simple.FromField (FromField)
-- import Database.PostgreSQL.Simple.FromRow (RowParser, field)
import GHC.Generics
       (Generic, Rep, K1(..), M1(..), (:*:)(..), from, to)
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)
import Generics.OneLiner
       (ADTRecord, Constraints, For(..), createA, gtraverse, op0)
import Streaming (Of, Stream)
import Streaming.Prelude (each)


import qualified OpaleyeStub as O
import OpaleyeStub (Connection, RowParser, field, FromField)

--------------------------------------------------------------------------------
-- | Indicate whether or not a column has a default value.
data HasDefault
  = HasDefault
  | NoDefault

--------------------------------------------------------------------------------
-- | Indicate whether or not a column can take default values.
data Nullable
  = Nullable
  | NotNullable

--------------------------------------------------------------------------------
{-| All metadata about a column in a table.

    'Column' is used to specify information about individual columns in base
    tables. While it is defined as a record, you construct 'Column's at the
    type level where record syntax is unfortunately not available.

    === __Example__

    @
    data Employee f =
      Employee { employeeName :: C f ('Column "employee_name" 'NoDefault 'NotNullable 'PGText) }
    @
-}
data Column t = Column
  { _columnName :: Symbol
  , _columnHasDefault :: HasDefault
  , _columnType :: t
  }

--------------------------------------------------------------------------------
-- | Database-side PostgreSQL expressions of a given type.
newtype Expr schema = Expr O.PrimExpr

--------------------------------------------------------------------------------
-- | Map a schema definition into a set of expressions that would select those
-- columns.
class InferBaseTableAttrExpr schema expr where
  baseTableAttrExpr :: schema a -> expr a

instance (InferBaseTableAttrExpr schema expr) =>
         InferBaseTableAttrExpr (M1 i c schema) (M1 i c expr) where
  baseTableAttrExpr (M1 s) = M1 (baseTableAttrExpr s)

instance ( InferBaseTableAttrExpr fSchema fExpr
         , InferBaseTableAttrExpr gSchema gExpr
         ) =>
         InferBaseTableAttrExpr (fSchema :*: gSchema) (fExpr :*: gExpr) where
  baseTableAttrExpr (l :*: r) = baseTableAttrExpr l :*: baseTableAttrExpr r

instance InferBaseTableAttrExpr (K1 i (Tagged name String)) (K1 i (Expr a)) where
  baseTableAttrExpr (K1 (Tagged name)) = K1 (Expr (O.BaseTableAttrExpr name))

--------------------------------------------------------------------------------
-- | Witness the schema definition for table columns.
class WitnessSchema a where
  schema :: a

instance KnownSymbol name =>
         WitnessSchema (Tagged name String) where
  schema = Tagged (symbolVal (Proxy :: Proxy name))

data Schema a

--------------------------------------------------------------------------------
class ToPrimExpr a where
  toPrimExpr :: a -> O.PrimExpr

instance ToPrimExpr (Expr column) where
  toPrimExpr (Expr a) = a

--------------------------------------------------------------------------------
-- TODO Unsure if we want to assume this type of table

class BaseTable (table :: (Column a -> *) -> *) where
  tableName :: proxy table -> String
  queryTable :: O.Query (table Expr)
  default queryTable :: ( ADTRecord (table Expr)
                        , ADTRecord (table Schema)
                        , Constraints (table Expr) ToPrimExpr
                        , Constraints (table Schema) WitnessSchema
                        , InferBaseTableAttrExpr (Rep (table Schema)) (Rep (table Expr))) =>
    O.Query (table Expr)
  queryTable =
    O.queryTableExplicit
      (O.ColumnMaker
         (O.PackMap
            (\f ->
               gtraverse (For :: For ToPrimExpr) (\s -> s <$ f (toPrimExpr s)))))
      (O.Table
         (tableName (Proxy :: Proxy table))
         (O.TableProperties
            (O.Writer (O.PackMap (\_ _ -> pure ())))
            (O.View
               (to
                  (baseTableAttrExpr
                     (from
                        (
                           (op0 (For :: For WitnessSchema) schema :: table Schema))))))))

--------------------------------------------------------------------------------
class Table expr haskell | expr -> haskell, haskell -> expr where
  rowParser :: expr -> RowParser haskell
  default rowParser :: ( ADTRecord haskell
                       , Constraints haskell FromField
                       , Generic haskell) =>
    expr -> RowParser haskell
  rowParser _ = head (createA (For :: For FromField) [field])

  unpackColumns :: O.Unpackspec expr expr
  default unpackColumns :: ( Generic expr
                           , Constraints expr ToPrimExpr
                           , ADTRecord expr) =>
    O.Unpackspec expr expr
  unpackColumns =
    O.Unpackspec
      (O.PackMap
         (\f ->
            gtraverse (For :: For ToPrimExpr) (\s -> s <$ f (toPrimExpr s))))

--------------------------------------------------------------------------------
type family C (f :: Column a -> *) (c :: Column a) :: * where
  C Expr ('Column _name _def t) = Expr t
  C QueryResult ('Column _name _def t) = t
  C Schema ('Column name _def _t) = Tagged name String

--------------------------------------------------------------------------------
data QueryResult column

--------------------------------------------------------------------------------
select
  :: (MonadIO m, Table rows results)
  => Connection -> O.Query rows -> Stream (Of results) m ()
select connection query = do
  results <-
    liftIO $
    O.runQueryExplicit
      (O.QueryRunner
         (void unpackColumns)
         rowParser
         (\_columns -> True) -- TODO Will we support 0-column queries?
       )
      connection
      query
  each results

--------------------------------------------------------------------------------
instance (Table lExpr lHaskell, Table rExpr rHaskell) =>
         Table (lExpr, rExpr) (lHaskell, rHaskell) where
  unpackColumns =
    (unpackColumns :: O.Unpackspec lExpr lExpr) ***!
    (unpackColumns :: O.Unpackspec rExpr rExpr)

  rowParser (l, r) = liftA2 (,) (rowParser l) (rowParser r)

--------------------------------------------------------------------------------
data MaybeRow row = MaybeRow (Expr Bool) row

instance (Table expr haskell) =>
         Table (MaybeRow expr) (Maybe haskell) where
  unpackColumns =
    dimap
      (\(MaybeRow tag row) -> (tag, row))
      (\(prim, expr) -> MaybeRow (Expr prim) expr)
      (O.Unpackspec (O.PackMap (\f (Expr tag) -> f tag)) ***! unpackColumns)

  rowParser (MaybeRow _ r) = do
    isNull <- field
    if fromMaybe True isNull
      then return Nothing
      else fmap Just (rowParser r)

(?) :: MaybeRow a -> (a -> Expr b) -> Expr (Maybe b)
MaybeRow _ row ? f = case f row of Expr a -> Expr a

--------------------------------------------------------------------------------
leftJoin
  :: (Table lExpr lHaskell, Table rExpr rHaskell)
  => (lExpr -> rExpr -> Expr Bool)
  -> O.Query lExpr
  -> O.Query rExpr
  -> O.Query (lExpr,MaybeRow rExpr)
leftJoin condition l r =
  O.leftJoinExplicit
    unpackColumns
    (unpackColumns ***! unpackColumns)
    (O.NullMaker (\(tag, t) -> MaybeRow tag t))
    l
    (liftA2 (,) (pure (lit False)) r)
    (\(a, (_, b)) ->
       case condition a b of
         Expr e -> O.Column e)

--------------------------------------------------------------------------------
class Lit a where
  lit :: a -> Expr a

instance Lit Bool where
  lit = Expr . O.ConstExpr . O.BoolLit

instance Lit Int where
  lit = Expr . O.ConstExpr . O.IntegerLit . fromIntegral

instance Lit a => Lit (Maybe a) where
  lit Nothing = Expr (O.ConstExpr O.NullLit)
  lit (Just a) =
    case lit a of
      Expr e -> Expr e

--------------------------------------------------------------------------------
newtype Col a = Col a

instance (FromField a) =>
         Table (Expr a) (Col a) where
  unpackColumns =
    O.Unpackspec (O.PackMap (\f (Expr prim) -> fmap Expr (f prim)))
  rowParser _ = fmap Col field

--------------------------------------------------------------------------------
(^/=^) :: Expr a -> Expr a -> Expr Bool
a ^/=^ b = undefined

toNullable :: Expr a -> Expr (Maybe a)
toNullable (Expr a) = Expr a

{- $intro

   Welcome to @rel8@!

   @rel8@ is a library that builds open the fantastic @opaleye@ to query
   databases. The main objectives of @rel8@ are:

   * /Conciseness/: Users using @rel8@ should not need to write boiler-plate
     code. By using expressive types, we can provide sufficient information
     for the compiler to infer code whenever possible.

   * /Inferrable/: Despite using a lot of type level magic, it should never
     be a requirement that the user must provide a type signature to allow a
     program to compile.

   With that said, let's dive in and see an example of a program using @rel8@.

   === Required language extensions and imports

   @
   { -# LANGUAGE
         Arrows, DataKinds, DeriveGeneric, FlexibleInstances,
         MultiParamTypeClasses #- }

   import Control.Applicative
   import Control.Arrow
   import Rel8
   @

   To use @rel8@, you will need a few language extensions:

   * @Arrows@ is necessary to use @proc@ notation. As with @opaleye@, @rel8@
     uses arrows to guarantee queries are valid.

   * @DataKinds@ is used to promote values to the type level when defining
     table/column metadata.

   * @DeriveGeneric@ is used to automatically derive functions from schema
     information.

   The others are used to provide the type system extensions needed by @rel8@.

   === Defining base tables

   In order to query a database of existing tables, we need to let @rel8@ know
   about these tables, and the schema for each table. This is done by defining
   a Haskell /record/ for each table in the database. These records should be
   parameterised by a type variable @f@, and each column is a field in this
   record, of type @C f ('Column ...)@. Let's see how that looks with some
   example tables:

   @
   data Part f =
     Part { partId     :: 'C' f (''Column' \"PID\" ''HasDefault' ''NotNullable' ''PGInteger')
          , partName   :: 'C' f (''Column' \"PName\" ''NoDefault' ''NotNullable' ''PGText')
          , partColor  :: 'C' f (''Column' \"Color\" ''NoDefault' ''NotNullable' ''PGInteger')
          , partWeight :: 'C' f (''Column' \"Weight\" ''NoDefault' ''NotNullable' ''PGReal')
          , partCity   :: 'C' f (''Column' \"City\" ''NoDefault' ''NotNullable' ''PGText')
          } deriving (Generic)

   instance 'BaseTable' Part where tableName = "Part"
   @

   The @Part@ table has 5 columns, each defined with the @C f ('Column ...)@
   pattern. For each column, we are specifying:

   1. The column name
   2. Whether or not this column has a default value when inserting new rows.
      In this case @partId@ does, as this is an auto-incremented primary key
      managed by the database.
   3. Whether or not the column can take @null@ values.
   4. The type of the column.

   === Querying tables

   With tables defined, we are now ready to write some queries. All base

-}
