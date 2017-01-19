{-# LANGUAGE Arrows #-}
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
  , Col(..)

    -- * Expressions
  , Expr

    -- ** Equality
  , DBEq, (==.), (?=.), in_, ilike

    -- ** Boolean-valued expressions
  , (&&.), (||.), not

    -- ** Literals
  , DBType(..)

    -- ** Null
  , toNullable , (?), isNull, nullable

    -- * Aggregation
  , AggregateTable, aggregate
  , count, groupBy, DBSum(..), countStar, DBMin(..), DBMax(..), avg
  , boolAnd, boolOr, stringAgg, arrayAgg
  , countRows

    -- * Querying Tables
  , select
  , QueryResult
  , label
  , distinct
  , where_
  , filterQuery
  , O.Query
  , Predicate

    -- * Modifying tables
  , insert, insert1Returning, insertReturning
  , update, updateReturning
  , delete
  , Default(..), Insert

    -- * Re-exported symbols
  , Connection, Stream, Of, Generic

    -- * Unsafe routines
  , unsafeCoerceExpr
  , dbFunction
  , nullaryFunction
  , dbBinOp
  ) where

import Control.Category ((.), id)
import Control.Applicative (liftA2)
import Control.Monad (void)
import Data.List (foldl')
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64)
import Data.Maybe (fromJust)
import Data.Maybe (fromMaybe)
import Data.Profunctor (dimap, lmap)
import Data.Profunctor.Product ((***!))
import Data.Proxy (Proxy(..))
import Data.Scientific (Scientific)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.FromRow (RowParser, field)
import GHC.Generics
       (Generic, Rep, K1(..), M1(..), (:*:)(..), from, to)
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)
import Generics.OneLiner
       (ADTRecord, Constraints, For(..), createA, gtraverse, nullaryOp)
import qualified Opaleye.Aggregate as O
import qualified Opaleye.Column as O
import qualified Opaleye.Internal.Aggregate as O
import qualified Opaleye.Internal.Column as O
import qualified Opaleye.Internal.Distinct as O
import qualified Opaleye.PGTypes as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.Join as O
import qualified Opaleye.Internal.PackMap as O
import qualified Opaleye.Internal.QueryArr as O
import qualified Opaleye.Internal.RunQuery as O
import qualified Opaleye.Internal.Table as O
import qualified Opaleye.Internal.TableMaker as O
import qualified Opaleye.Internal.Unpackspec as O
import qualified Opaleye.Join as O
import Opaleye.Label (label)
import qualified Opaleye.Manipulation as O
import qualified Opaleye.Operators as O
import qualified Opaleye.RunQuery as O
import qualified Opaleye.Table as O hiding (required)
import Prelude hiding (not, (.), id)
import Streaming (Of, Stream)
import Streaming.Prelude (each)
import qualified Streaming.Prelude as S

infix 4 ==.
infix 4 ?=.
infixr 2 ||.
infixr 2 &&.

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
class Writer schema expr where
  columnWriter :: schema a -> O.Writer (expr a) ()

instance (Writer schema expr) =>
         Writer (M1 i c schema) (M1 i c expr) where
  columnWriter (M1 s) = lmap (\(M1 a) -> a) (columnWriter s)

instance (Writer fSchema fExpr, Writer gSchema gExpr) =>
         Writer (fSchema :*: gSchema) (fExpr :*: gExpr) where
  columnWriter (l :*: r) =
    dimap (\(l' :*: r') -> (l', r')) fst (columnWriter l ***! columnWriter r)

instance Writer (K1 i (Tagged name String)) (K1 i (Expr a)) where
  columnWriter (K1 (Tagged name)) =
    dimap
      (\(K1 (Expr a)) -> O.Column a)
      (const ())
      (O.required name)

instance Writer (K1 i (Tagged name String)) (K1 i (Default (Expr a))) where
  columnWriter (K1 (Tagged name)) =
    dimap
      (\(K1 def) ->
         case def of
           InsertDefault -> O.Column O.DefaultInsertExpr
           OverrideDefault (Expr a) -> O.Column a)
      (const ())
      (O.required name)

--------------------------------------------------------------------------------
-- | Witness the schema definition for table columns.
class WitnessSchema a where
  schema :: a

instance KnownSymbol name =>
         WitnessSchema (Tagged name String) where
  schema = Tagged (symbolVal (Proxy :: Proxy name))

data Schema a

--------------------------------------------------------------------------------
class MapPrimExpr s where
  mapPrimExpr :: Applicative f => (O.PrimExpr -> f O.PrimExpr) -> s -> f s

instance MapPrimExpr (Expr column) where
  mapPrimExpr f (Expr a) = fmap Expr (f a)

--------------------------------------------------------------------------------
-- TODO Unsure if we want to assume this type of table

-- | 'BaseTable' @name record@ specifies that there is a table named @name@, and
-- the record type @record@ specifies the columns of that table.
class (KnownSymbol name, Table (table Expr) (table QueryResult)) =>
      BaseTable (name :: Symbol) (table :: (* -> *) -> *) | table -> name where
  -- | Query all rows in a table
  queryTable :: O.Query (table Expr)
  queryTable =
    O.queryTableExplicit
      (O.ColumnMaker (O.PackMap traversePrimExprs))
      tableDefinition

  tableDefinition :: O.Table (table Insert) (table Expr)
  default
    tableDefinition :: ( ADTRecord (table Expr)
                       , ADTRecord (table Schema)
                       , Constraints (table Schema) WitnessSchema
                       , InferBaseTableAttrExpr (Rep (table Schema)) (Rep (table Expr))
                       , Writer (Rep (table Schema)) (Rep (table Insert))
                       , Generic (table Insert)
                       )
                    => O.Table (table Insert) (table Expr)
  tableDefinition =
    O.Table
         (symbolVal (Proxy :: Proxy name))
         (O.TableProperties
            (case lmap from (columnWriter (from tableSchema)) of
               O.Writer f -> O.Writer f)
            (O.View (to (baseTableAttrExpr (from tableSchema)))))
    where
      tableSchema :: table Schema
      tableSchema = nullaryOp (For :: For WitnessSchema) schema

  -- TODO Would really like to reconcile this with tableDefinition
  tableDefinitionUpdate :: O.Table (table Expr) (table Expr)
  default
    tableDefinitionUpdate :: ( ADTRecord (table Expr)
                             , ADTRecord (table Schema)
                             , Constraints (table Schema) WitnessSchema
                             , InferBaseTableAttrExpr (Rep (table Schema)) (Rep (table Expr))
                             , Writer (Rep (table Schema)) (Rep (table Expr))
                             )
                          => O.Table (table Expr) (table Expr)
  tableDefinitionUpdate =
    O.Table
         (symbolVal (Proxy :: Proxy name))
         (O.TableProperties
            (case lmap from (columnWriter (from tableSchema)) of
               O.Writer f -> O.Writer f)
            (O.View (to (baseTableAttrExpr (from tableSchema)))))
    where
      tableSchema :: table Schema
      tableSchema = nullaryOp (For :: For WitnessSchema) schema

--------------------------------------------------------------------------------
-- | 'Table' @expr haskell@ specifies that the @expr@ contains one or more
-- 'Expr' columns, and when this table is queried using 'select' it returns
-- the type @haskell@.
--
-- 'Table's are not necessarily concrete tables within a database. For example,
-- the join of two 'Table's (as witness by tuple construction) is itself a
-- 'Table'.
class Table expr haskell | expr -> haskell, haskell -> expr where
  rowParser :: RowParser haskell
  default rowParser :: ( ADTRecord haskell
                       , Constraints haskell FromField
                       ) =>
    RowParser haskell
  rowParser = head (createA (For :: For FromField) [field])

  traversePrimExprs :: Applicative f => (O.PrimExpr -> f O.PrimExpr) -> expr -> f expr
  default traversePrimExprs :: ( Constraints expr MapPrimExpr
                               , ADTRecord expr
                               , Applicative f
                               )
                            => (O.PrimExpr -> f O.PrimExpr) -> expr -> f expr
  traversePrimExprs f = gtraverse (For :: For MapPrimExpr) (mapPrimExpr f)

unpackColumns :: Table expr haskell => O.Unpackspec expr expr
unpackColumns = O.Unpackspec (O.PackMap traversePrimExprs)

instance {-# OVERLAPPABLE #-}
         ( ADTRecord (table Expr)
         , ADTRecord (table QueryResult)
         , Constraints (table Expr) MapPrimExpr
         , Constraints (table QueryResult) FromField
         ) => Table (table Expr) (table QueryResult)

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
  C Insert name 'HasDefault t = Default (Expr t)
  C Insert name 'NoDefault t = Expr t

data Default a
  = OverrideDefault a
  | InsertDefault

--------------------------------------------------------------------------------
-- | Interpret a 'Table' as Haskell values.
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
      queryRunner
      connection
      query
  each results

queryRunner :: Table a b => O.QueryRunner a b
queryRunner =
  O.QueryRunner (void unpackColumns)
                (const rowParser)
                (\_columns -> True) -- TODO Will we support 0-column queries?


--------------------------------------------------------------------------------
instance (Table lExpr lHaskell, Table rExpr rHaskell) =>
         Table (lExpr, rExpr) (lHaskell, rHaskell) where
  traversePrimExprs f (l, r) =
    liftA2 (,) (traversePrimExprs f l) (traversePrimExprs f r)

  rowParser = liftA2 (,) rowParser rowParser

--------------------------------------------------------------------------------
-- | Indicates that a given 'Table' might be @null@. This is the result of a
-- @LEFT JOIN@ between tables.
data MaybeTable row = MaybeTable (Expr Bool) row

instance (Table expr haskell) =>
         Table (MaybeTable expr) (Maybe haskell) where
  traversePrimExprs f (MaybeTable (Expr tag) row) =
    MaybeTable <$> (Expr <$> f tag) <*> traversePrimExprs f row

  rowParser = do
    isNull <- field
    if fromMaybe True isNull
      then return Nothing
      else fmap Just rowParser

-- | Project an expression out of a 'MaybeTable', preserving the fact that this
-- column might be @null@.
(?) :: ToNullable b maybeB => MaybeTable a -> (a -> Expr b) -> Expr maybeB
MaybeTable _ row ? f = toNullable (f row)

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
       case toNullableBool (condition a b) of
         Expr e -> O.Column e)

--------------------------------------------------------------------------------
-- | The class of Haskell values that can be mapped to database types.
class DBType a where
  -- | Lift a Haskell value into a literal database expression.
  lit :: a -> Expr a

instance DBType Bool where
  lit = columnToExpr . O.pgBool

instance DBType Char where
  lit = Expr . O.ConstExpr . O.StringLit . pure

instance DBType Int16 where
  lit = Expr . O.ConstExpr . O.IntegerLit . fromIntegral

instance DBType Int32 where
  lit = columnToExpr . O.pgInt4 . fromIntegral

instance DBType Int64 where
  lit = columnToExpr . O.pgInt8

instance DBType Double where
  lit = columnToExpr . O.pgDouble

instance DBType Float where
  lit = Expr . O.ConstExpr . O.DoubleLit . realToFrac

instance DBType a => DBType (Maybe a) where
  lit Nothing = Expr (O.ConstExpr O.NullLit)
  lit (Just a) =
    case lit a of
      Expr e -> Expr e

instance DBType Text where
  lit = columnToExpr . O.pgStrictText

instance DBType ByteString where
  lit = columnToExpr . O.pgStrictByteString

instance DBType UTCTime where
  lit = columnToExpr . O.pgUTCTime

columnToExpr :: O.Column a -> Expr b
columnToExpr (O.Column a) = Expr a

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
  deriving (Show)

instance (FromField a) =>
         Table (Expr a) (Col a) where
  traversePrimExprs f (Expr a) = Expr <$> f a
  rowParser = fmap Col field

--------------------------------------------------------------------------------
-- | Lift an 'Expr' to be nullable. Like the 'Just' constructor.
--
-- If an Expr is already nullable, then this acts like the identity function.
-- This is useful as it allows projecting an already-nullable column from a left
-- join.
class ToNullable a maybeA | a -> maybeA where
  toNullable :: Expr a -> Expr maybeA

instance ToNullableHelper a maybeA (IsMaybe a) => ToNullable a maybeA where
  toNullable = toNullableHelper (Proxy :: Proxy (IsMaybe a))

class ToNullableHelper a maybeA join | join a -> maybeA where
  toNullableHelper :: proxy join -> Expr a -> Expr maybeA

instance ToNullableHelper a (Maybe a) 'False where
  toNullableHelper _ = unsafeCoerceExpr @(Maybe a)

instance ToNullableHelper (Maybe a) (Maybe a) 'True where
  toNullableHelper _ = id

class Predicate a where
  toNullableBool :: Expr a -> Expr (Maybe Bool)

instance Predicate Bool where
  toNullableBool = toNullable

instance Predicate (Maybe Bool) where
  toNullableBool = id

--------------------------------------------------------------------------------
type family IsMaybe (a :: *) :: Bool where
  IsMaybe (Maybe a) = 'True
  IsMaybe _ = 'False

--------------------------------------------------------------------------------
-- | The class of types that can be compared for equality within the database.
class DBType a => DBEq a where
  (==.) :: Expr a -> Expr a -> Expr Bool
  Expr a ==. Expr b = Expr (O.BinExpr (O.:==) a b)

  (?=.) :: Expr (Maybe a) -> Expr (Maybe a) -> Expr (Maybe Bool)
  a ?=. b = toNullable (unsafeCoerceExpr @a a ==. unsafeCoerceExpr @a b)

instance DBEq Bool where
instance DBEq Char where
instance DBEq Double where
instance DBEq Float where
instance DBEq Int16 where
instance DBEq Int32 where
instance DBEq Int64 where
instance DBEq Text where

--------------------------------------------------------------------------------
class Booleanish a where
  not :: a -> a
  (&&.) :: a -> a -> a
  (||.) :: a -> a -> a

instance Booleanish (Expr Bool) where
  not (Expr a) = Expr (O.UnExpr O.OpNot a)
  Expr a &&. Expr b = Expr (O.BinExpr O.OpAnd a b)
  Expr a ||. Expr b = Expr (O.BinExpr O.OpOr a b)

instance Booleanish (Expr (Maybe Bool)) where
  not = unsafeCoerceExpr . not . unsafeCoerceExpr @Bool
  a &&. b = unsafeCoerceExpr (unsafeCoerceExpr @Bool a &&. unsafeCoerceExpr @Bool b)
  a ||. b = unsafeCoerceExpr (unsafeCoerceExpr @Bool a ||. unsafeCoerceExpr @Bool b)

unsafeCoerceExpr :: forall b a. Expr a -> Expr b
unsafeCoerceExpr (Expr a) = Expr a

--------------------------------------------------------------------------------
-- | Used to tag 'Expr's that are the result of aggregation
data Aggregate a = Aggregate (Maybe O.AggrOp) O.PrimExpr

count :: Expr a -> Aggregate Int64
count (Expr a) = Aggregate (Just O.AggrCount) a

groupBy :: Expr a -> Aggregate a
groupBy (Expr a) = Aggregate Nothing a

countStar :: Aggregate Int64
countStar = count (lit @Int64 0)

class DBAvg a res | a -> res where
  avg :: Expr a -> Aggregate res
  avg (Expr a) = Aggregate (Just O.AggrAvg) a

instance DBAvg Int64 Scientific
instance DBAvg Double Double
instance DBAvg Int32 Scientific
instance DBAvg Scientific Scientific
instance DBAvg Int16 Scientific

-- | The class of data types that can be aggregated under the @sum@ operation.
class DBSum a res | a -> res where
  sum :: Expr a -> Aggregate b
  sum (Expr a) = Aggregate (Just O.AggrSum) a

instance DBSum Int64 Scientific
instance DBSum Double Double
instance DBSum Int32 Int64
instance DBSum Scientific Scientific
instance DBSum Float Float
instance DBSum Int16 Int64

class DBMax a where
  max :: Expr a -> Aggregate a
  max (Expr a) = Aggregate (Just O.AggrMax) a

class DBMin a where
  min :: Expr a -> Aggregate a
  min (Expr a) = Aggregate (Just O.AggrMin) a

boolOr :: Expr Bool -> Aggregate Bool
boolOr (Expr a) = Aggregate (Just O.AggrBoolOr) a

boolAnd :: Expr Bool -> Aggregate Bool
boolAnd (Expr a) = Aggregate (Just O.AggrBoolAnd) a

arrayAgg :: Expr a -> Aggregate [a]
arrayAgg (Expr a) = Aggregate (Just O.AggrArr) a

stringAgg :: Expr String -> Expr String -> Aggregate String
stringAgg (Expr combiner) (Expr a) = Aggregate (Just (O.AggrStringAggr combiner)) a

countRows :: O.Query a -> O.Query (Expr Int64)
countRows = fmap (\(O.Column a) -> Expr a) . O.countRows

class AggregateTable columns result | columns -> result, result -> columns where
  aggregator :: O.Aggregator columns result

instance AggregateTable (Aggregate a) (Expr a) where
  aggregator =
    O.Aggregator
      (O.PackMap
         (\f (Aggregate op ex) -> fmap Expr (f (liftA2 (,) op (pure []), ex))))

instance (AggregateTable a1 b1, AggregateTable a2 b2) =>
         AggregateTable (a1, a2) (b1, b2) where
  aggregator = aggregator ***! aggregator

aggregate
  :: AggregateTable table result
  => O.Query table -> O.Query result
aggregate = O.aggregate aggregator

distinct :: Table table haskell => O.Query table -> O.Query table
distinct =
  O.distinctExplicit
    (O.Distinctspec
       (O.Aggregator (O.PackMap (\f -> traversePrimExprs (\e -> f (Nothing,e))))))

--------------------------------------------------------------------------------
data Insert a

insert
  :: (BaseTable tableName table, MonadIO m)
  => Connection -> [table Insert] -> m Int64
insert conn rows =
  liftIO (O.runInsertMany conn tableDefinition rows)

insertReturning
  :: (BaseTable tableName table, MonadIO m)
  => Connection -> [table Insert] -> Stream (Of (table QueryResult)) m ()
insertReturning conn rows =
  do results <-
       liftIO (O.runInsertManyReturningExplicit queryRunner conn tableDefinition rows id)
     each results

insert1Returning
  :: (BaseTable tableName table,MonadIO m)
  => Connection -> table Insert -> m (table QueryResult)
insert1Returning c = fmap fromJust . S.head_ . insertReturning c . pure

update
  :: (BaseTable tableName table, Predicate bool, MonadIO m)
  => Connection
  -> (table Expr -> Expr bool)
  -> (table Expr -> table Expr)
  -> m Int64
update conn f up =
  liftIO $
  O.runUpdate
    conn
    tableDefinitionUpdate
    up
    (\rel ->
       case toNullableBool (f rel) of
         Expr a -> O.Column a)

updateReturning
  :: (BaseTable tableName table, Predicate bool, MonadIO m)
  => Connection
  -> (table Expr -> Expr bool)
  -> (table Expr -> table Expr)
  -> Stream (Of (table QueryResult)) m ()
updateReturning conn f up =
  do r <-
       liftIO $
       O.runUpdateReturningExplicit
         queryRunner
         conn
         tableDefinitionUpdate
         up
         (\rel ->
            case toNullableBool (f rel) of
              Expr a -> O.Column a)
         id
     each r

-- | Given a 'BaseTable' and a predicate, @DELETE@ all rows that match.
delete
  :: (BaseTable name table, Predicate bool)
  => Connection
  -> (table Expr -> Expr bool)
  -> IO Int64
delete conn f =
  O.runDelete
    conn
    tableDefinition
    (\rel ->
       case toNullableBool (f rel) of
         Expr a -> O.Column a)

where_ :: Predicate bool => O.QueryArr (Expr bool) ()
where_ = lmap toNullableBool (lmap (\(Expr a) -> O.Column a) O.restrict)

filterQuery :: Predicate bool => (a -> Expr bool) -> O.Query a -> O.Query a
filterQuery f q = proc _ -> do
  row <- q -< ()
  where_ -< f row
  id -< row

isNull :: Expr (Maybe a) -> Expr Bool
isNull = undefined

in_ :: DBEq a => Expr a -> [Expr a] -> Expr Bool
in_ x = foldl' (\b y -> x ==. y ||. b) (lit False)

ilike :: Expr Text -> Expr Text -> Expr Bool
Expr a `ilike` Expr b =
  case O.binOp (O.OpOther "ILIKE") (O.Column a) (O.Column b) of
    O.Column c -> Expr c

--------------------------------------------------------------------------------
class Function arg res where
  -- | Build a function of multiple arguments.
  mkFunctionGo :: ([O.PrimExpr] -> O.PrimExpr) -> arg -> res

instance (DBType a, arg ~ Expr a) =>
         Function arg (Expr res) where
  mkFunctionGo mkExpr (Expr a) = Expr (mkExpr [a])

instance (DBType a, arg ~ Expr a, Function args res) =>
         Function arg (args -> res) where
  mkFunctionGo f (Expr a) = mkFunctionGo (f . (a :))

dbFunction :: Function args result => String -> args -> result
dbFunction = mkFunctionGo . O.FunExpr

nullaryFunction :: DBType a => String -> Expr a
nullaryFunction name = Expr (O.FunExpr name [])

-- | Eliminate 'PGNull' from the type of an 'Expr'. Like 'maybe' for Haskell
-- values.
nullable
  :: Expr b -> (Expr a -> Expr b) -> Expr (Maybe a) -> Expr b
nullable (Expr a) f (Expr e) =
  case O.matchNullable
         (O.Column a)
         (\(O.Column x) ->
            case f (Expr x) of
              Expr x' -> O.Column x')
         (O.Column e) of
    O.Column b -> Expr b

dbBinOp :: String -> Expr a -> Expr b -> Expr c
dbBinOp op (Expr a) (Expr b) =
  case O.binOp (O.OpOther op) (O.Column a) (O.Column b) of
    O.Column c -> Expr c

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
