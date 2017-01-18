{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Rel8
  ( -- $intro

    -- * Defining Tables
    C
  , HasDefault(..)
  , Nullable(..)
  , BaseTable(..)

    -- * Tables
  , Table(..)
  , leftJoin
  , MaybeTable(..)
  , Col

    -- * Expressions
  , Expr

    -- ** Equality
  , Eq, (==), (/=), DBEq

    -- ** Boolean-valued expressions
  , (&&), (||), not

    -- * DBTypeerals
  , DBType(..)

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

import Prelude hiding (Eq, (==), (&&), (||), not, (/=))
import qualified Prelude

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

--------------------------------------------------------------------------------
-- | Database-side PostgreSQL expressions of a given type.
newtype Expr (t :: *) = Expr O.PrimExpr

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

-- | 'BaseTable' @name record@ specifies that there is a table named @name@, and
-- the record type @record@ specifies the columns of that table.
class KnownSymbol name =>
      BaseTable (name :: Symbol) (table :: (* -> *) -> *) | table -> name where
  -- | Query all rows in a table
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
         (symbolVal (Proxy :: Proxy name))
         (O.TableProperties
            (O.Writer (O.PackMap (\_ _ -> pure ())))
            (O.View
               (to
                  (baseTableAttrExpr
                     (from
                        ((op0 (For :: For WitnessSchema) schema :: table Schema))))))))

--------------------------------------------------------------------------------
-- | 'Table' @expr haskell@ specifies that the @expr@ contains one or more
-- 'Expr' columns, and when this table is queried using 'select' it returns
-- the type @haskell@.
--
-- 'Table's are not necessarily concrete tables within a database. For example,
-- the join of two 'Table's (as witness by tuple construction) is itself a
-- 'Table'.
class Table expr haskell | expr -> haskell, haskell -> expr where
  rowParser :: expr -> RowParser haskell
  default rowParser :: ( ADTRecord haskell
                       , Constraints haskell FromField
                       ) =>
    expr -> RowParser haskell
  rowParser _ = head (createA (For :: For FromField) [field])

  unpackColumns :: O.Unpackspec expr expr
  default unpackColumns :: ( Constraints expr ToPrimExpr
                           , ADTRecord expr) =>
    O.Unpackspec expr expr
  unpackColumns =
    O.Unpackspec
      (O.PackMap
         (\f ->
            gtraverse (For :: For ToPrimExpr) (\s -> s <$ f (toPrimExpr s))))

instance {-# OVERLAPPABLE #-}
         ( ADTRecord (table Expr)
         , ADTRecord (table QueryResult)
         , Constraints (table Expr) ToPrimExpr
         , Constraints (table QueryResult) FromField
         ) =>
         Table (table Expr) (table QueryResult)

--------------------------------------------------------------------------------
{-| All metadata about a column in a table.

    'C' is used to specify information about individual columns in base
    tables. While it is defined as a record, you construct 'Column's at the
    type level where record syntax is unfortunately not available.

    === __Example__

    @
    data Employee f =
      Employee { employeeName :: C f ('Column "employee_name" 'NoDefault 'NotNullable 'PGText) }
    @
-}
type family C (f :: * -> *) (columnName :: Symbol) (hasDefault :: HasDefault) (columnType :: t) :: * where
  C Expr _name _def t = Expr t
  C QueryResult _name _def t = t
  C Schema name _def _t = Tagged name String

--------------------------------------------------------------------------------
data QueryResult column

--------------------------------------------------------------------------------
-- | Given a database query, execute this query and return a 'Stream' of
-- results.
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
-- | Indicates that a given 'Table' might be @null@. This is the result of a
-- @LEFT JOIN@ between tables.
data MaybeTable row = MaybeTable (Expr Bool) row

instance (Table expr haskell) =>
         Table (MaybeTable expr) (Maybe haskell) where
  unpackColumns =
    dimap
      (\(MaybeTable tag row) -> (tag, row))
      (\(prim, expr) -> MaybeTable (Expr prim) expr)
      (O.Unpackspec (O.PackMap (\f (Expr tag) -> f tag)) ***! unpackColumns)

  rowParser (MaybeTable _ r) = do
    isNull <- field
    if fromMaybe True isNull
      then return Nothing
      else fmap Just (rowParser r)

-- | Project an expression out of a 'MaybeTable', preserving the fact that this
-- column might be @null@.
(?) :: MaybeTable a -> (a -> Expr b) -> Expr (Maybe b)
MaybeTable _ row ? f = case f row of Expr a -> Expr a

--------------------------------------------------------------------------------
-- | Take the @LEFT JOIN@ of two tables.
leftJoin
  :: (Table lExpr lHaskell, Table rExpr rHaskell, Predicate bool)
  => (lExpr -> rExpr -> Expr bool) -- ^ The condition to join upon.
  -> O.Query lExpr -- ^ The left table
  -> O.Query rExpr -- ^ The right table
  -> O.Query (lExpr,MaybeTable rExpr)
leftJoin condition l r =
  O.leftJoinExplicit
    unpackColumns
    (unpackColumns ***! unpackColumns)
    (O.NullMaker (\(tag, t) -> MaybeTable tag t))
    l
    (liftA2 (,) (pure (lit False)) r)
    (\(a, (_, b)) ->
       case condition a b of
         Expr e -> O.Column e)

--------------------------------------------------------------------------------
-- | The class of Haskell values that can be mapped to database types.
class DBType a where
  -- | Lift a Haskell value into a literal database expression.
  lit :: a -> Expr a

instance DBType Bool where
  lit = Expr . O.ConstExpr . O.BoolLit

instance DBType Int where
  lit = Expr . O.ConstExpr . O.IntegerLit . fromIntegral

instance DBType a => DBType (Maybe a) where
  lit Nothing = Expr (O.ConstExpr O.NullLit)
  lit (Just a) =
    case lit a of
      Expr e -> Expr e

--------------------------------------------------------------------------------
{- | A one column 'Table' of type @a@. This type is required for queries that
   return only one column (for reasons of preserving type inference). It can
   also be used to build "anonymous" tables, by joining multiple tables with
   tupling.

   === Example: Querying a single column

   @
   data TestTable f = TestTable { col :: Col f "col" 'NoDefault Int}

   oneCol :: Stream (Of (Col Int))
   oneCol = select connection $ testColumn <$> queryTable
   @

   === Example: Building tables out of single columns

   @
   data T1 f = TestTable { col1 :: Col f "col" 'NoDefault Int}
   data T2 f = TestTable { col2 :: Col f "col" 'NoDefault Bool}

   q :: Stream (Of (Col Int, Col Bool))
   q = select connection $ proc () -> do
     t1 <- queryTable -< ()
     t2 <- queryTable -< ()
     returnA -< (col1 t1, col2 t2)
   @
-}
newtype Col a = Col a

instance (FromField a) =>
         Table (Expr a) (Col a) where
  unpackColumns =
    O.Unpackspec (O.PackMap (\f (Expr prim) -> fmap Expr (f prim)))
  rowParser _ = fmap Col field

--------------------------------------------------------------------------------
-- | Lift an 'Expr' to be nullable. Like the 'Just' constructor.
toNullable :: Expr a -> Expr (Maybe a)
toNullable = unsafeCoerceExpr

class Predicate a where
instance Predicate Bool where
instance Predicate (Maybe Bool) where

--------------------------------------------------------------------------------
data Origin = Rel8 | OtherType

type family OriginOf (a :: *) :: Origin where
  OriginOf (Expr a) = 'Rel8
  OriginOf _ = 'OtherType

--------------------------------------------------------------------------------
-- | Prelude's 'Prelude.Eq' constraint.
type Eq a = (OriginOf a ~ 'OtherType, HEq a Bool)

-- | Overloaded equality. This equality can be used as a drop in for normal
-- equality as provided by Prelude, but has special behavior for 'Expr':
--
-- * @(==) :: Expr a -> Expr a -> Expr Bool@
-- * @(==) :: Expr (Maybe a) -> Expr (Maybe a) -> Expr (Maybe Bool)@
class HEq operand result | operand -> result where
  (==) :: operand -> operand -> result
  (/=) :: operand -> operand -> result

instance HEqHelper operand result (OriginOf operand) => HEq operand result where
  (==) = eq (Proxy :: Proxy (OriginOf operand))
  {-# INLINE (==) #-}

  (/=) = neq (Proxy :: Proxy (OriginOf operand))
  {-# INLINE (/=) #-}

class HEqHelper operand result origin | operand origin -> result where
  eq :: proxy origin -> operand -> operand -> result
  neq :: proxy origin -> operand -> operand -> result

instance Prelude.Eq a => HEqHelper a Bool 'OtherType where
  eq _ = (Prelude.==)
  {-# INLINE eq #-}

  neq _ = (Prelude./=)
  {-# INLINE neq #-}

instance (Booleanish (Expr result), NullableEq operand result (IsMaybe operand)) =>
         HEqHelper (Expr operand) (Expr result) 'Rel8 where
  eq _ = nullableEq (Proxy :: Proxy (IsMaybe operand))
  {-# INLINE eq #-}

  neq _ = \a b -> not (nullableEq (Proxy :: Proxy (IsMaybe operand)) a b)
  {-# INLINE neq #-}

type family IsMaybe (a :: *) :: Bool where
  IsMaybe (Maybe a) = 'True
  IsMaybe _ = 'False

class NullableEq operand result (isNull :: Bool) | operand isNull -> result where
  nullableEq :: proxy isNull -> Expr operand -> Expr operand -> Expr result

instance DBEq a => NullableEq (Maybe a) (Maybe Bool) 'True where
  nullableEq _ (Expr a) (Expr b) = Expr (O.BinExpr (O.:==) a b)
  {-# INLINE nullableEq #-}

instance NullableEq a Bool 'False where
  nullableEq _ (Expr a) (Expr b) = Expr (O.BinExpr (O.:==) a b)
  {-# INLINE nullableEq #-}

-- | The class of types that can be compared for equality within the database.
class DBEq a
instance DBEq Int
instance DBEq Bool

--------------------------------------------------------------------------------
class Booleanish a where
  not :: a -> a
  (&&) :: a -> a -> a
  (||) :: a -> a -> a

instance Booleanish Bool where
  not = Prelude.not
  (&&) = (Prelude.&&)
  (||) = (Prelude.||)

instance Booleanish (Expr Bool) where
  not (Expr a) = Expr (O.UnExpr O.OpNot a)
  Expr a && Expr b = Expr (O.BinExpr O.OpAnd a b)
  Expr a || Expr b = Expr (O.BinExpr O.OpOr a b)

instance Booleanish (Expr (Maybe Bool)) where
  not = unsafeCoerceExpr . not . unsafeCoerceExpr @Bool
  a && b = unsafeCoerceExpr (unsafeCoerceExpr @Bool a && unsafeCoerceExpr @Bool b)
  a || b = unsafeCoerceExpr (unsafeCoerceExpr @Bool a || unsafeCoerceExpr @Bool b)

unsafeCoerceExpr :: forall b a. Expr a -> Expr b
unsafeCoerceExpr (Expr a) = Expr a

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
   a Haskell /record/ for each table in the database. These records should have
   a type of the form @C f name hasDefault t@. Let's see how that looks with some
   example tables:

   @
   data Part f =
     Part { partId     :: 'C' f \"PID\" ''HasDefault' Int
          , partName   :: 'C' f \"PName\" ''NoDefault' Text
          , partColor  :: 'C' f \"Color\" ''NoDefault' Int
          , partWeight :: 'C' f \"Weight\" ''NoDefault' Double
          , partCity   :: 'C' f \"City\" ''NoDefault' Text
          } deriving (Generic)

   instance 'BaseTable' "part" Part
   @

   The @Part@ table has 5 columns, each defined with the @C f ('Column ...)@
   pattern. For each column, we are specifying:

   1. The column name
   2. Whether or not this column has a default value when inserting new rows.
      In this case @partId@ does, as this is an auto-incremented primary key
      managed by the database.
   3. Whether or not the column can take @null@ values.
   4. The type of the column.

   After defining the table, we finally need to make an instance of 'BaseTable'
   so @rel8@ can query this table. By using @deriving (Generic)@, we simply need
   to write @instance BaseTable "part" Part@. The string @"part"@ here is the
   name of the table in the database (which could differ from the name of the
   type @Part@).

   === Querying tables

   With tables defined, we are now ready to write some queries. All base

-}
