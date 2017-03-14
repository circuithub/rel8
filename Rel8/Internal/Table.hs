{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DataKinds #-}

-- | This module defines the 'Table' type class.
module Rel8.Internal.Table where

import Data.Proxy
import GHC.TypeLits
import Control.Applicative
import Control.Lens (Iso', from, iso, view)
import Control.Monad (replicateM_)
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (traverse_)
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Rep (Representable, index, tabulate, pureRep)
import Data.Maybe (fromMaybe)
import Data.Profunctor (dimap)
import Data.Tagged (Tagged(..), untag)
import Database.PostgreSQL.Simple.FromRow (RowParser, field)
import GHC.Generics
       ((:*:)(..), Generic, K1(..), M1(..), Rep, to)
import GHC.Generics.Lens (generic, _M1, _K1)
import Generics.OneLiner (nullaryOp, ADTRecord, Constraints, For(..))
import qualified Opaleye.Aggregate as O
import qualified Opaleye.Column as O
import qualified Opaleye.Internal.Aggregate as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.PackMap as O
import qualified Opaleye.Internal.QueryArr as O
import qualified Opaleye.Internal.Table as O
import qualified Opaleye.Internal.TableMaker as O
import qualified Opaleye.Internal.Unpackspec as O
import qualified Opaleye.Table as O hiding (required)
import Prelude hiding (not)
import Prelude hiding (not, id)
import Rel8.Internal.DBType
import Rel8.Internal.Expr
import Rel8.Internal.Types

type family MkRowF a :: * -> * where
  MkRowF (M1 i c f) = MkRowF f
  MkRowF (a :*: b) = Product (MkRowF a) (MkRowF b)
  MkRowF (K1 i c) = RowF c

--------------------------------------------------------------------------------

-- | 'Table' @expr haskell@ specifies that the @expr@ contains one or more
-- 'Expr' columns, and when this table is queried using 'select' it returns
-- the type @haskell@.
--
-- 'Table's are not necessarily concrete tables within a database. For example,
-- the join of two 'Table's (as witnessed by tuple construction) is itself a
-- 'Table' - but cannot be inserted as it doesn't belong to any base table.
--
-- You should not need to define your own instances of 'Table' in idiomatic
-- @rel8@ usage - beyond simplying deriving an instance and using generics.
class (Representable (RowF expr), Traversable (RowF expr)) =>
      Table expr haskell | expr -> haskell, haskell -> expr where

  -- | Every 'Table' is isomorphic to a 'Functor' with finite cardinality
  -- (a 'Representable' functor). This type witnesses what that functor
  -- is. By default, this functor can be derived from the generic representation
  -- of @expr@.
  type RowF expr :: * -> *
  type RowF expr = MkRowF (Rep expr)

  -- | Witness the isomorphism between the expression's functor of primitive
  -- expressions, and the user friendly expression type. A default
  -- implementation is provided, assuming every field in @expr@ contains
  -- exactly one 'Expr'.
  expressions :: Iso' expr (RowF expr O.PrimExpr)

  -- Every expression can be parsed into Haskell, once results have been
  -- retrieved from the database. A generic implementation is provided which
  -- assumes that every field in @haskell@ represents exactly one column
  -- in the result.
  rowParser :: RowParser haskell

  default
    rowParser
      :: (Generic haskell, GTable (Rep expr) (Rep haskell))
      => RowParser haskell
  rowParser = fmap to growParser

  default
    expressions
      :: ( Generic expr
         , GTable (Rep expr) (Rep haskell)
         , RowF expr ~ MkRowF (Rep expr)
         )
      => Iso' expr (RowF expr O.PrimExpr)
  expressions = generic . gexpressions


--------------------------------------------------------------------------------
class GTable expr haskell | expr -> haskell, haskell -> expr where
  growParser :: RowParser (haskell a)
  gexpressions :: Iso' (expr a) (MkRowF expr O.PrimExpr)

instance GTable expr haskell => GTable (M1 i c expr) (M1 i c haskell) where
  growParser = M1 <$> growParser
  gexpressions = _M1 . gexpressions

instance (GTable le lh, GTable re rh) =>
         GTable (le :*: re) (lh :*: rh) where
  growParser = liftA2 (:*:) growParser growParser
  gexpressions =
    iso
      (\(l :*: r) -> Pair (view gexpressions l) (view gexpressions r))
      (\(Pair l r) ->
         view (from gexpressions) l :*: view (from gexpressions) r)

instance {-# OVERLAPPABLE #-}
         Table expr haskell => GTable (K1 i expr) (K1 i haskell) where
  growParser = K1 <$> rowParser
  gexpressions = _K1 . expressions

instance DBType a =>
         GTable (K1 i (Expr a)) (K1 i a) where
  growParser = K1 <$> field
  gexpressions =
    iso
      (\(K1 (Expr prim)) -> Identity prim)
      (\(Identity prim) -> K1 (Expr prim))

--------------------------------------------------------------------------------
-- Stock instances of 'Table'

instance (Table a a', Table b b') => Table (a, b) (a', b') where
  type RowF (a, b) = Product (RowF a) (RowF b)

  expressions =
    dimap
      (\(l, r) -> Pair (view expressions l) (view expressions r))
      (fmap
         (\(Pair l r) ->
            (view (from expressions) l, view (from expressions) r)))

  rowParser = (,) <$> rowParser <*> rowParser

instance (Table a a', Table b b', Table c c') =>
         Table (a, b, c) (a', b', c') where
  type RowF (a, b, c) = Product (RowF (a, b)) (RowF c)

  expressions =
    dimap
      (\(a, b, c) ->
         Pair
           (Pair (view expressions a) (view expressions b))
           (view expressions c))
      (fmap
         (\(Pair (Pair a b) c) ->
            ( view (from expressions) a
            , view (from expressions) b
            , view (from expressions) c)))

  rowParser = (,,) <$> rowParser <*> rowParser <*> rowParser

instance (Table a a', Table b b', Table c c', Table d d') =>
         Table (a, b, c, d) (a', b', c', d') where
  type RowF (a, b, c, d) = Product (RowF (a, b, c)) (RowF d)

  expressions =
    dimap
      (\(a, b, c, d) ->
         Pair
           (Pair
              (Pair (view expressions a) (view expressions b))
              (view expressions c))
           (view expressions d))
      (fmap
         (\(Pair (Pair (Pair a b) c) d) ->
            ( view (from expressions) a
            , view (from expressions) b
            , view (from expressions) c
            , view (from expressions) d)))

  rowParser =
    (,,,) <$> rowParser
          <*> rowParser
          <*> rowParser
          <*> rowParser

instance (Table a a', Table b b', Table c c', Table d d', Table e e') =>
         Table (a, b, c, d, e) (a', b', c', d', e') where
  type RowF (a, b, c, d, e) = Product (RowF (a, b, c, d)) (RowF e)

  expressions =
    dimap
      (\(a, b, c, d, e) ->
         Pair
           (Pair
              (Pair
                 (Pair (view expressions a) (view expressions b))
                 (view expressions c))
              (view expressions d))
           (view expressions e))
      (fmap
         (\(Pair (Pair (Pair (Pair a b) c) d) e) ->
            ( view (from expressions) a
            , view (from expressions) b
            , view (from expressions) c
            , view (from expressions) d
            , view (from expressions) e)))

  rowParser =
    (,,,,) <$> rowParser
           <*> rowParser
           <*> rowParser
           <*> rowParser
           <*> rowParser


--------------------------------------------------------------------------------
-- | Indicates that a given 'Table' might be @null@. This is the result of a
-- @LEFT JOIN@ between tables.
data MaybeTable row = MaybeTable (Expr (Maybe Bool)) row
  deriving (Functor)

-- | The result of a left/right join is a table, but the table may be entirely
-- @null@ sometimes.
instance (Table expr haskell) => Table (MaybeTable expr) (Maybe haskell) where
  type RowF (MaybeTable expr) = Product Identity (RowF expr)

  expressions =
    dimap
      (\(MaybeTable tag row) ->
         Pair (view expressions tag) (view expressions row))
      (fmap
         (\(Pair tag row) ->
            MaybeTable
              (view (from expressions) tag)
              (view (from expressions) row)))

  rowParser = do
    isNull' <- field
    if fromMaybe True isNull'
      then Nothing <$
           replicateM_
             (length (pureRep () :: RowF expr ()))
             (field :: RowParser (Maybe ()))
      else fmap Just rowParser

-- | Project an expression out of a 'MaybeTable', preserving the fact that this
-- column might be @null@. Like field selection.
--
-- It may be helpful to remember this operator by the mneumonic - '$' on the left
-- means function on the left, '?' on the right means 'MaybeTable' on the right.
infixl 4 $?
($?) :: ToNullable b maybeB => (a -> Expr b) -> MaybeTable a -> Expr maybeB
f $? MaybeTable _ x = toNullable (f x)

-- | Check if a 'MaybeTable' is a @NULL@ row. Usually this means a @LEFT JOIN@
-- that did match any rows.
isTableNull :: MaybeTable a -> Expr Bool
isTableNull (MaybeTable tag _) = nullable (lit True) (\_ -> lit False) tag

--------------------------------------------------------------------------------
-- | Eliminate 'Maybe' from the type of an 'Expr'. Like 'maybe' for Haskell
-- values.
nullable
  :: Expr b -> (Expr a -> Expr b) -> Expr (Maybe a) -> Expr b
nullable a f b =
  columnToExpr
    (O.matchNullable
       (exprToColumn a)
       (exprToColumn . f . columnToExpr)
       (exprToColumn b))


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
newtype Col a = Col { unCol :: a }
  deriving (Show, ToJSON, FromJSON, Read, Eq, Ord)

-- | Single 'Expr'essions are tables, but the result will be wrapped in 'Col'.
instance DBType a => Table (Expr a) (Col a) where
  type RowF (Expr a) = Identity
  expressions = dimap (\(Expr a) -> return a) (fmap (Expr . runIdentity))
  rowParser = fmap Col field

--------------------------------------------------------------------------------
traversePrimExprs
  :: (Applicative f, Table expr haskell)
  => (O.PrimExpr -> f O.PrimExpr) -> expr -> f expr
traversePrimExprs f expr =
  expressions (traverse f) expr


--------------------------------------------------------------------------------
unpackColumns :: Table expr haskell => O.Unpackspec expr expr
unpackColumns = O.Unpackspec (O.PackMap traversePrimExprs)


--------------------------------------------------------------------------------
-- | A 'BaseTable' is a table that is specified directly in a relational
-- database schema with @CREATE TABLE@. This allows you to both @SELECT FROM@
-- rows from this table, @UPDATE@ and @DELETE@ existing rows and @INSERT@ new
-- rows.

class Table (table Expr) (table QueryResult) => BaseTable table where
  -- | The name of this table in the database. You can use the 'FromString'
  -- instance for 'Tagged' to simply write
  -- @tableName = "employees"@, for example.
  tableName :: Tagged table String

  -- | Witness the schema of a table at the value level.
  columns :: table SchemaInfo

  -- | Any 'BaseTable' is isomorphic to a set of interpretations. These
  -- interpretations can be viewed as colimits if we throw away the type
  -- information.
  tabular
    :: Interpretation f
    -> Iso' (table f) (RowF (table Expr) (Colimit f))

  ------------------------------------------------------------------------------

  default
    columns
      :: ( ADTRecord (table SchemaInfo)
         , Constraints (table SchemaInfo) WitnessSchema
         )
      => table SchemaInfo
  columns = nullaryOp (For :: For WitnessSchema) schema

  default
    tabular
      :: ( Generic (table f)
         , GTabular Expr (Rep (table Expr)) (RowF (table Expr))
         , GTabular SchemaInfo (Rep (table SchemaInfo)) (RowF (table Expr))
         , GTabular Insert (Rep (table Insert)) (RowF (table Expr))
         )
      => Interpretation f -> Iso' (table f) (RowF (table Expr) (Colimit f))
  tabular AsExpr = generic . gtabular AsExpr
  tabular AsSchemaInfo = generic . gtabular AsSchemaInfo
  tabular AsInsert = generic . gtabular AsInsert

--------------------------------------------------------------------------------
class GTabular i repIn repOut where
  gtabular :: Interpretation i -> Iso' (repIn a) (repOut (Colimit i))

instance GTabular i repIn repOut => GTabular i (M1 meta c repIn) repOut where
  gtabular i = _M1 . gtabular i

instance (GTabular i inl outl, GTabular i inr outr) =>
         GTabular i (inl :*: inr) (Product outl outr) where
  gtabular i =
    iso
      (\(a :*: b) -> Pair (view (gtabular i) a) (view (gtabular i) b))
      (\(Pair a b) -> view (from (gtabular i)) a :*: view (from (gtabular i)) b)

instance GTabular Expr (K1 i (Expr a)) Identity where
  gtabular _ =
    _K1 . iso (Identity . Colimit) (\(Identity (Colimit (Expr a))) -> Expr a)

instance GTabular SchemaInfo (K1 i (SchemaInfo (a :: k))) Identity where
  gtabular _ =
    _K1 .
    iso
      (\(SchemaInfo a) -> Identity (Colimit (SchemaInfo a)))
      (\(Identity (Colimit (SchemaInfo a))) -> SchemaInfo a)

-- This is only proper if OverrideDefault DefaultInsertExpr can't occur.
instance GTabular Insert (K1 i (Default (Expr a))) Identity where
  gtabular _ = _K1 . iso forward backward
    where
      forward (OverrideDefault a) = Identity (Colimit (InsertExpr a))
      forward InsertDefault =
        Identity (Colimit (InsertExpr (Expr O.DefaultInsertExpr)))
      backward (Identity (Colimit (InsertExpr (Expr O.DefaultInsertExpr)))) =
        InsertDefault
      backward (Identity (Colimit (InsertExpr (Expr prim)))) =
        OverrideDefault (Expr prim)

instance GTabular Insert (K1 i (Expr a)) Identity where
  gtabular _ = _K1 . iso forward backward
    where
      forward = Identity . Colimit . InsertExpr
      backward (Identity (Colimit (InsertExpr (Expr a)))) = Expr a


--------------------------------------------------------------------------------
viewTable
  :: forall table.
     BaseTable table
  => table Expr
viewTable =
  view
    (from expressions)
    (fmap
       (\(Colimit (SchemaInfo str)) -> O.BaseTableAttrExpr str)
       (view (tabular AsSchemaInfo) (columns @table)))


--------------------------------------------------------------------------------
-- | Query all rows in a table. Equivalent to @SELECT * FROM table@.
queryTable :: BaseTable table => O.Query (table Expr)
queryTable =
  O.queryTableExplicit
    (O.ColumnMaker (O.PackMap traversePrimExprs))
    tableDefinition

--------------------------------------------------------------------------------
tableDefinition
  :: forall table.
     BaseTable table
  => O.Table (table Insert) (table Expr)
tableDefinition =
  O.Table
    (untag @table tableName)
    (O.TableProperties
       (O.Writer
          (O.PackMap
             (\f a ->
                let columnName =
                      index
                        (fmap
                           (\(Colimit (SchemaInfo str)) -> str)
                           (view (tabular AsSchemaInfo) (columns @table)))
                    exprs =
                      fmap
                        (fmap (\(Colimit (InsertExpr (Expr x))) -> x) .
                         view (tabular AsInsert))
                        a
                    foo rep = f (fmap (`index` rep) exprs, columnName rep)
                in traverse_ id (tabulate @(RowF (table Expr)) foo))))
       (O.View viewTable))

tableDefinitionUpdate
  :: forall table.
     BaseTable table
  => O.Table (table Expr) (table Expr)
tableDefinitionUpdate =
  O.Table
    (untag @table tableName)
    (O.TableProperties
       (O.Writer
          (O.PackMap
             (\f a ->
                let columnName =
                      index
                        (fmap
                           (\(Colimit (SchemaInfo str)) -> str)
                           (view (tabular AsSchemaInfo) (columns @table)))
                    exprs = fmap (view expressions) a
                    foo rep = f (fmap (`index` rep) exprs, columnName rep)
                in traverse_ id (tabulate @(RowF (table Expr)) foo))))
       (O.View viewTable))


--------------------------------------------------------------------------------
-- | 'AggregateTable' is used to demonstrate that a table only contains
-- aggregation or @GROUP BY@ expressions. If you wish to use your own records
-- for aggregation results, parameterise the record over @f@, use 'Anon' to
-- specify the columns, and then generically derive 'AggregateTable'
class AggregateTable columns result | columns -> result, result -> columns where
  aggregations :: Iso' columns (RowF result (Limit Aggregate))

  default
    aggregations
      :: (GTraverseAggregator (Rep columns) (Rep result), Generic columns)
      => Iso' columns (MkRowF (Rep result) (Limit Aggregate))
  aggregations = generic . gaggregations

-- | A single column aggregates to a single expression.
instance AggregateTable (Aggregate a) (Expr a) where
  aggregations =
    iso
      (\(Aggregate a e) -> Identity (Limit (Aggregate a e)))
      (runLimit . runIdentity)

instance (AggregateTable a1 b1, AggregateTable a2 b2) =>
         AggregateTable (a1, a2) (b1, b2) where
  aggregations =
    iso
      (\(a, b) -> Pair (view aggregations a) (view aggregations b))
      (\(Pair l r) ->
         (view (from aggregations) l, view (from aggregations) r))

--------------------------------------------------------------------------------
class GTraverseAggregator aggregator expr | aggregator -> expr where
  gaggregations
    :: Iso' (aggregator x) (MkRowF expr (Limit Aggregate))

instance (GTraverseAggregator aggregator f) =>
         GTraverseAggregator (M1 i c aggregator) (M1 i c f) where
  gaggregations = _M1 . gaggregations

instance ( GTraverseAggregator fAggregator fExpr
         , GTraverseAggregator gAggregator gExpr
         ) =>
         GTraverseAggregator (fAggregator :*: gAggregator) (fExpr :*: gExpr) where
  gaggregations =
    iso
      (\(a :*: b) -> Pair (view gaggregations a) (view gaggregations b))
      (\(Pair a b) ->
         view (from gaggregations) a :*: view (from gaggregations) b)

instance AggregateTable a b => GTraverseAggregator (K1 i a) (K1 i b) where
  gaggregations = _K1 . aggregations


--------------------------------------------------------------------------------
-- | Evaluate aggregation over a query. The 'AggregateTable' constraint
-- requires that all columns in each row must be grouped or aggregated.
aggregate
  :: (AggregateTable table result, Table result haskell)
  => O.Query table -> O.Query result
aggregate =
  O.aggregate $
  O.Aggregator $
  O.PackMap $ \f ->
    fmap (view (from expressions)) .
    traverse (\(Limit (Aggregate a e)) -> f (a, e)) . view aggregations

--------------------------------------------------------------------------------
-- | Witness the schema definition for table columns.
class WitnessSchema a where
  schema :: a

instance KnownSymbol name =>
         WitnessSchema (SchemaInfo '(name, (def :: k), (t :: j))) where
  schema = SchemaInfo (symbolVal (Proxy @name))
