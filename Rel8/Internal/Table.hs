{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the 'Table' type class.
module Rel8.Internal.Table where

import Data.String (IsString(..))
import Control.Applicative
import Control.Lens (AnIso', Iso', cloneIso, from, iso, view)
import Control.Monad.Trans.State.Strict ( State, runState, state )
import Data.Bifunctor ( Bifunctor, bimap )
import Data.Foldable (traverse_)
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Rep (Representable, index, liftR3, mzipWithRep, tabulate, pureRep)
import qualified Data.Functor.Rep as Representable
import Data.Kind ( Type )
import Data.Maybe (fromMaybe)
import Data.Profunctor (dimap)
import Data.Proxy
import Data.Tagged (Tagged(..), untag)
import Data.These ( These( This, That, These ) )
import Database.PostgreSQL.Simple.FromRow (RowParser, field, fieldWith)
import GHC.Generics
       ((:*:)(..), Generic, K1(..), M1(..), Meta(..), Rep, S1, to)
import GHC.Generics.Lens (generic, _M1, _K1)
import GHC.TypeLits
import Generics.OneLiner (nullaryOp, ADTRecord, Constraints)
import qualified Opaleye.Aggregate as O
import qualified Opaleye.Column as O
import qualified Opaleye.Internal.Aggregate as O
import qualified Opaleye.Internal.Binary as O
import qualified Opaleye.Internal.Column as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.PackMap as O
import qualified Opaleye.Internal.PrimQuery as O ( Bindings, PrimQuery'( Rebind ) )
import qualified Opaleye.Internal.QueryArr as O
import qualified Opaleye.Internal.Table as O
import qualified Opaleye.Internal.Tag as O
import qualified Opaleye.Internal.Unpackspec as O
import qualified Opaleye.Internal.Values as O
import qualified Opaleye.PGTypes as O
import qualified Opaleye.Table as O hiding (required)
import Prelude hiding (not, null)
import Rel8.Internal.DBType
import Rel8.Internal.Expr
import Rel8.Internal.Operators
import Rel8.Internal.Types

type family MkRowF a :: * -> * where
  MkRowF (M1 i c f) = MkRowF f
  MkRowF (a :*: b) = Product (MkRowF a) (MkRowF b)
  MkRowF (K1 i c) = RowF c


class ExprFor expr haskell

instance {-# OVERLAPPABLE #-} a ~ Expr b => ExprFor a b
instance (a ~ t Expr, queryResult ~ QueryResult) => ExprFor a (t queryResult)
instance ExprFor a b => ExprFor (MaybeTable a) (Maybe b)
instance (a ~ (a1, a2)) => ExprFor a (b1, b2)
instance (a ~ (a1, a2, a3)) => ExprFor a (b1, b2, b3)
instance (a ~ (a1, a2, a3, a4)) => ExprFor a (b1, b2, b3, b4)
instance (a ~ (a1, a2, a3, a4, a5)) => ExprFor a (b1, b2, b3, b4, b5)


class (Representable (HRowF t), Traversable (HRowF t)) => HigherKindedTable t where
  type HRowF t :: * -> *
  type HRowF t = MkRowF (Rep (t Expr))

  hexpressions :: AnIso' (t Expr) (HRowF t O.PrimExpr)
  default hexpressions
    :: ( HRowF t ~ MkRowF (Rep (t Expr))
       , Generic (t Expr)
       , GTable (Rep (t Expr)) (Rep (t QueryResult))
       ) => AnIso' (t Expr) (HRowF t O.PrimExpr)
  hexpressions = generic . gexpressions @(Rep (t Expr)) @(Rep (t QueryResult))


  hrowParser :: RowParser (t QueryResult)
  default hrowParser
    :: ( GTable (Rep (t Expr)) (Rep (t QueryResult))
       , Generic (t QueryResult)
       )
    => RowParser (t QueryResult)
  hrowParser = fmap to (growParser @(Rep (t Expr)) @(Rep (t QueryResult)))


  hlit :: t QueryResult -> t Expr
  default hlit
    :: ( GTable (Rep (t Expr)) (Rep (t QueryResult))
       , Generic (t QueryResult)
       , Generic (t Expr)
       )
    => t QueryResult -> t Expr
  hlit = to . glit . view generic


  hbindings :: t Name -> t Expr -> State O.Tag (O.Bindings O.PrimExpr, t Expr)
  default hbindings
    :: ( GBindings (Rep (t Name)) (Rep (t Expr))
       , Generic (t Name)
       , Generic (t Expr)
       )
    => t Name -> t Expr -> State O.Tag (O.Bindings O.PrimExpr, t Expr)
  hbindings names exprs =
    fmap to <$> gbindings (view generic names) (view generic exprs)


  hdefaultNames :: (String -> String) -> t Name
  default hdefaultNames
    :: ( GDefaultNames (Rep (t Name))
       , Generic (t Name)
       )
    => (String -> String) -> (t Name)
  hdefaultNames = to . gdefaultNames


instance (s ~ t, expr ~ Expr, HigherKindedTable t, queryResult ~ QueryResult) => Table (s expr) (t queryResult) where
  type RowF (s expr) = HRowF s
  expressions_ _ = cloneIso hexpressions
  rowParser_ _ = hrowParser
  tlit = hlit


class GTable expr haskell where
  growParser :: RowParser (haskell a)
  gexpressions :: Iso' (expr a) (MkRowF expr O.PrimExpr)
  glit :: haskell a -> expr a


instance GTable expr haskell => GTable (M1 i c expr) (M1 i c haskell) where
  growParser = M1 <$> growParser @expr @haskell
  gexpressions = _M1 . gexpressions @expr @haskell
  glit (M1 x) = M1 $ glit x


instance (GTable le lh, GTable re rh) =>
         GTable (le :*: re) (lh :*: rh) where
  growParser = liftA2 (:*:) (growParser @le @lh) (growParser @re @rh)
  glit (x :*: y) = glit x :*: glit y
  gexpressions =
    iso
      (\(l :*: r) -> Pair (view (gexpressions @le @lh) l) (view (gexpressions @re @rh) r))
      (\(Pair l r) ->
         view (from (gexpressions @le @lh)) l :*: view (from (gexpressions @re @rh)) r)


instance {-# OVERLAPPABLE #-}
         Table expr haskell => GTable (K1 i expr) (K1 i haskell) where
  growParser = K1 <$> rowParser @expr
  glit (K1 x) = K1 $ lit x
  gexpressions = _K1 . expressions


instance DBType a =>
         GTable (K1 i (Expr a)) (K1 i a) where
  glit (K1 x) = K1 $ lit x
  growParser = K1 <$> field
  gexpressions =
    iso
      (\(K1 (Expr prim)) -> Identity prim)
      (\(Identity prim) -> K1 (Expr prim))

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
class (Representable (RowF expr), Traversable (RowF expr), ExprFor expr haskell) => Table expr haskell | expr -> haskell where

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
  expressions_ :: Proxy haskell -> Iso' expr (RowF expr O.PrimExpr)

  -- | Every expression can be parsed into Haskell, once results have been
  -- retrieved from the database. A generic implementation is provided which
  -- assumes that every field in @haskell@ represents exactly one column
  -- in the result.
  rowParser_ :: Proxy expr -> RowParser haskell

  default
    rowParser_
      :: (Generic haskell, GTable (Rep expr) (Rep haskell))
      => Proxy expr -> RowParser haskell
  rowParser_ _ = fmap to (growParser @(Rep expr) @(Rep haskell))

  default
    expressions_
      :: ( Generic expr
         , GTable (Rep expr) (Rep haskell)
         , RowF expr ~ MkRowF (Rep expr)
         )
      => Proxy haskell -> Iso' expr (RowF expr O.PrimExpr)
  expressions_ _ = generic . gexpressions @(Rep expr) @(Rep haskell)

  tlit :: haskell -> expr
  default tlit
    :: (Generic haskell, GTable (Rep expr) (Rep haskell), Generic expr)
    => haskell -> expr
  tlit = to . glit . view generic


expressions :: forall expr haskell. Table expr haskell => Iso' expr (RowF expr O.PrimExpr)
expressions = expressions_ (Proxy @haskell)


rowParser :: forall expr haskell. Table expr haskell => RowParser haskell
rowParser = rowParser_ (Proxy @expr)


bool :: forall bool a haskell. (Predicate bool, Table a haskell) => a -> a -> Expr bool -> a
bool as bs predicate = view (from expressions) $ mzipWithRep go
  (view expressions as)
  (view expressions bs)
  where
    go false true = a
      where
        Expr a = case_ [(predicate, Expr true)] (Expr false)


--------------------------------------------------------------------------------
-- | Indicates that a given 'Table' might be @null@. This is the result of a
-- @LEFT JOIN@ between tables.
data MaybeTable row = MaybeTable (Expr (Maybe Bool)) row
  deriving Functor

instance Applicative MaybeTable where
  pure = MaybeTable (lit (Just False))
  MaybeTable t f <*> MaybeTable t' a = MaybeTable (liftOpNull (||.) t t') (f a)

instance Monad MaybeTable where
  MaybeTable t a >>= f = case f a of
    MaybeTable t' b -> MaybeTable (liftOpNull (||.) t t') b

-- | The result of a left/right join is a table, but the table may be entirely
-- @null@ sometimes.
instance (Table expr haskell) => Table (MaybeTable expr) (Maybe haskell) where
  type RowF (MaybeTable expr) = Product Identity (RowF expr)

  expressions_ _ =
    dimap
      (\(MaybeTable tag row) ->
         Pair
           (view expressions tag)
           (view expressions row))
      (fmap
         (\(Pair tag row) ->
            MaybeTable
              (view (from expressions) tag)
              (view (from expressions) row)))

  rowParser_ _ = do
    isNull' <- field
    if fromMaybe True isNull'
      then Nothing <$
           sequence_ (pureRep @(RowF expr) (fieldWith (\_ _ -> pure ())))
      else fmap Just (rowParser @expr)

  tlit = maybe nothingTable (pure . lit)


-- | Project an expression out of a 'MaybeTable', preserving the fact that this
-- column might be @null@. Like field selection.
--
-- It may be helpful to remember this operator by the mneumonic - '$' on the left
-- means function on the left, '?' on the right means 'MaybeTable' on the right.
infixl 4 $?
($?) :: forall a b maybeB. (DBType maybeB, ToNullable b maybeB)
  => (a -> Expr b) -> MaybeTable a -> Expr maybeB
f $? ma = maybeTable (null @(UnMaybe maybeB)) (toNullable . f) ma


-- | Check if a 'MaybeTable' is a @NULL@ row. Usually this means a @LEFT JOIN@
-- that did match any rows.
isTableNull :: MaybeTable a -> Expr Bool
isTableNull (MaybeTable tag _) = nullable (lit True) (\_ -> lit False) tag


maybeTable :: Table b haskell
  => b -> (a -> b) -> MaybeTable a -> b
maybeTable b f (MaybeTable tag a) =
  view (from expressions) $ mzipWithRep (ifNull tag)
    (view expressions b)
    (view expressions (f a))
  where
    ifNull :: Expr (Maybe Bool) -> O.PrimExpr -> O.PrimExpr -> O.PrimExpr
    ifNull conditional true false = unColumn $ O.matchNullable
      (O.Column true)
      (\_ -> O.Column false)
      (exprToColumn conditional)
      where
        unColumn (O.Column result) = result


nothingTable :: Table a haskell => MaybeTable a
nothingTable =
  MaybeTable null (view (from expressions) (pureRep (O.ConstExpr O.NullLit)))


altTable :: Table a haskell => MaybeTable a -> MaybeTable a -> MaybeTable a
altTable ma@(MaybeTable tag a) (MaybeTable tag' b) =
  MaybeTable (bool tag tag' (isTableNull ma)) (bool a b (isTableNull ma))


--------------------------------------------------------------------------------
-- | A pair of 'Table's where at most one might be @null@. This is the result of
-- an @FULL OUTER JOIN@ between tables.
data TheseTable a b = TheseTable (MaybeTable a) (MaybeTable b)
  deriving Functor


instance Bifunctor TheseTable where
  bimap f g (TheseTable a b) = TheseTable (fmap f a) (fmap g b)


-- -- | The result of a full outer join is a pair of tables, but one of the tables
-- -- may be entirely @null@ sometimes.
-- instance (Table exprA a, Table exprB b) => Table (TheseTable exprA exprB) (These a b) where
--   type RowF (TheseTable exprA exprB) =
--     Product (RowF (MaybeTable exprA)) (RowF (MaybeTable exprB))

--   tlit _ = \case
--     This a    -> TheseTable (lit @(MaybeTable a) (Just a)) (lit Nothing)
--     That b    -> TheseTable (lit @(MaybeTable a) Nothing)  (lit (Just b))
--     These a b -> TheseTable (lit @(MaybeTable a) (Just a)) (lit (Just b))

--   expressions_ _ = dimap back (fmap forth)
--     where
--       back (TheseTable a b) =
--          Pair
--            (view expressions a)
--            (view expressions b)
--       forth (Pair a b) =
--         TheseTable
--           (view (from expressions) a)
--           (view (from expressions) b)

--   rowParser_ _ = do
--     ma <- rowParser
--     mb <- rowParser
--     case (ma, mb) of
--       (Just a, Just b) -> pure $ These a b
--       (Just a, _) -> pure $ This a
--       (_, Just b) -> pure $ That b
--       _ -> empty


theseTable :: Table c haskell
  => (a -> c) -> (b -> c) -> (a -> b -> c) -> TheseTable a b -> c
theseTable f g h (TheseTable (MaybeTable aNull a) (MaybeTable bNull b)) =
  view (from expressions) $ liftR3 go
    (view expressions (f a))
    (view expressions (g b))
    (view expressions (h a b))
  where
    ifNull :: Expr (Maybe Bool) -> O.PrimExpr -> O.PrimExpr -> O.PrimExpr
    ifNull conditional true false = unColumn $ O.matchNullable
      (O.Column true)
      (\_ -> O.Column false)
      (exprToColumn conditional)
      where
        unColumn (O.Column result) = result

    go this that these = ifNull bNull this (ifNull aNull that these)


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
-- | Single 'Expr'essions are tables.
instance DBType a => Table (Expr a) a where
  type RowF (Expr a) = Identity
  expressions_ _ = dimap (\(Expr a) -> return a) (fmap (Expr . runIdentity))
  rowParser_ _ = field
  tlit = Expr . formatLit (dbTypeInfo @a)

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
valuesColumns :: Table expr haskell => O.ValuesspecUnsafe expr expr
valuesColumns = O.Valuesspec $ O.PackMap $ dimap id $
  fmap (view (from expressions)) . sequenceA . pureRep


--------------------------------------------------------------------------------
binaryColumns :: Table expr haskell => O.Binaryspec expr expr
binaryColumns = O.Binaryspec $ O.PackMap $ \f (l, r) ->
  fmap (view (from expressions)) . sequenceA $ mzipWithRep (curry f)
    (view expressions l)
    (view expressions r)


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

  -- | The name of the schema in the database. If the schema is defined,
  -- the queries will schema-qualify tables as defined in Opaleye.
  tableSchema :: Maybe ( Tagged table String )

  -- | Witness the schema of a table at the value level.
  columns :: table SchemaInfo

  -- | Any 'BaseTable' is isomorphic to a set of interpretations. These
  -- interpretations can be viewed as colimits if we throw away the type
  -- information.
  tabular
    :: Interpretation f
    -> Iso' (table f) (RowF (table Expr) (Colimit f))

  ------------------------------------------------------------------------------

  tableSchema = Nothing

  default
    columns
      :: ( ADTRecord (table SchemaInfo)
         , Constraints (table SchemaInfo) WitnessSchema
         )
      => table SchemaInfo
  columns = nullaryOp @WitnessSchema schema

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

instance GTabular SchemaInfo (K1 i (SchemaInfo '( (a :: Symbol), (b :: HasDefault), c))) Identity where
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
       (view (tabular @table AsSchemaInfo) (columns @table)))


--------------------------------------------------------------------------------
-- | Query all rows in a table. Equivalent to @SELECT * FROM table@.
queryTable :: forall table. BaseTable table => O.Query (table Expr)
queryTable =
  O.queryTableExplicit
    (O.Unpackspec (O.PackMap (traversePrimExprs @_ @(table Expr))))
    (tableDefinition @table)

--------------------------------------------------------------------------------
tableDefinition
  :: forall table. BaseTable table
  => O.Table (table Insert) (table Expr)
tableDefinition =
  tableDefinition_
    (fmap (\(Colimit (InsertExpr (Expr x))) -> x) . view (tabular @table AsInsert))

tableDefinitionUpdate
  :: BaseTable table
  => O.Table (table Expr) (table Expr)
tableDefinitionUpdate = tableDefinition_ (view expressions)

tableDefinition_
  :: forall f table.
     (BaseTable table)
  => (table f -> RowF (table Expr) O.PrimExpr) -> O.Table (table f) (table Expr)
tableDefinition_ toExprs =
  defineTable
    (untag @table tableName)
    (O.TableFields (O.Writer (O.PackMap go)) (O.View viewTable))

  where

    defineTable
      :: String
      -> O.TableFields writeFields viewFields
      -> O.Table writeFields viewFields
    defineTable =
      case tableSchema of
        Nothing ->
          O.Table

        Just schemaName ->
          O.TableWithSchema (untag @table schemaName)

    go
      :: forall exprs g. (Functor exprs, Applicative g)
      => ((exprs O.PrimExpr, String) -> g ()) -> exprs (table f) -> g ()
    go f a =
      let columnName :: Representable.Rep (RowF (table Expr)) -> String
          columnName =
            index
              (fmap
                 (\(Colimit (SchemaInfo str)) -> str)
                 (view (tabular @table AsSchemaInfo) (columns @table)))

          exprs :: exprs (RowF (table Expr) O.PrimExpr)
          exprs = fmap toExprs a

          exprWithColumns :: Representable.Rep (RowF (table Expr)) -> g ()
          exprWithColumns rep = f (fmap (`index` rep) exprs, columnName rep)

      in traverse_ id (tabulate @(RowF (table Expr)) exprWithColumns)

--------------------------------------------------------------------------------
-- | 'AggregateTable' is used to demonstrate that a table only contains
-- aggregation or @GROUP BY@ expressions. If you wish to use your own records
-- for aggregation results, parameterise the record over @f@, use 'Anon' to
-- specify the columns, and then generically derive 'AggregateTable'
class AggregateTable columns result | columns -> result, result -> columns where
  aggregations :: Iso' columns (RowF result (Limit Aggregate))

  default
    aggregations
      :: (GTraverseAggregator (Rep columns) (Rep result), Generic columns, MkRowF (Rep result) (Limit Aggregate) ~ RowF result (Limit Aggregate))
      => Iso' columns (RowF result (Limit Aggregate))
  aggregations = generic . gaggregations

-- | A single column aggregates to a single expression.
instance AggregateTable (Aggregate a) (Expr a) where
  aggregations =
    iso
      (\(Aggregate a e) -> Identity (Limit (Aggregate a e)))
      (runLimit . runIdentity)

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
         WitnessSchema (SchemaInfo '(name, (def :: HasDefault), t)) where
  schema = SchemaInfo (symbolVal (Proxy @name))

--------------------------------------------------------------------------------
-- Table products as tuples

instance (Table a a', Table b b') =>
         Table (a, b) (a', b')
instance (Table a a', Table b b', Table c c') =>
         Table (a, b, c) (a', b', c')
instance (Table a a', Table b b', Table c c', Table d d') =>
         Table (a, b, c, d) (a', b', c', d')
instance (Table a a', Table b b', Table c c', Table d d', Table e e') =>
         Table (a, b, c, d, e) (a', b', c', d', e')

instance (AggregateTable a a', AggregateTable b b') =>
         AggregateTable (a, b) (a', b')
instance (AggregateTable a a', AggregateTable b b', AggregateTable c c') =>
         AggregateTable (a, b, c) (a', b', c')
instance (AggregateTable a a', AggregateTable b b', AggregateTable c c', AggregateTable d d') =>
         AggregateTable (a, b, c, d) (a', b', c', d')
instance (AggregateTable a a', AggregateTable b b', AggregateTable c c', AggregateTable d d', AggregateTable e e') =>
         AggregateTable (a, b, c, d, e) (a', b', c', d', e')


--------------------------------------------------------------------------------
instance (IsString a, DBType a) => IsString (Expr a) where
  fromString = lit . fromString


--------------------------------------------------------------------------------
-- | Lift a Haskell value into a literal database expression.
lit :: forall haskell expr. Table expr haskell => haskell -> expr
lit = tlit 


instance {-# OVERLAPS#-} (IsString a, DBType a) => IsString (Expr (Maybe a)) where
  fromString = lit . Just . fromString


-- | It is assumed that any Haskell types that have a 'Num' instance also have
-- the corresponding operations in the database. Hence, Num a => Num (Expr a).
-- *However*, if this is not the case, you should `newtype` the Haskell type
-- and avoid providing a 'Num' instance, or you may write be able to write
-- ill-typed queries!
instance (DBType a, Num a) => Num (Expr a) where
  a + b = columnToExpr (O.binOp (O.:+) (exprToColumn a) (exprToColumn b))
  a * b = columnToExpr (O.binOp (O.:*) (exprToColumn a) (exprToColumn b))
  abs = dbFunction "abs"
  signum = columnToExpr @O.PGInt8 . signum . exprToColumn
  fromInteger = lit . fromInteger
  negate = columnToExpr @O.PGInt8 . negate . exprToColumn


instance (DBType a, Fractional a) => Fractional (Expr a) where
  a / b = columnToExpr (O.binOp (O.:/) (exprToColumn a) (exprToColumn b))
  fromRational = lit . fromRational


class GBindings name expr | expr -> name, name -> expr where
  gbindings :: name a -> expr a -> State O.Tag (O.Bindings O.PrimExpr, expr a)


instance GBindings name expr => GBindings (M1 i c name) (M1 i c expr) where
  gbindings (M1 names) (M1 exprs) = fmap M1 <$> gbindings names exprs


instance (GBindings ln le, GBindings rn re) =>
         GBindings (ln :*: rn) (le :*: re) where
  gbindings (inames :*: inames') (iexprs :*: iexprs') = do
    (onames, oexprs) <- gbindings inames iexprs
    (onames', oexprs') <- gbindings inames' iexprs'
    pure (onames <> onames', oexprs :*: oexprs')


instance Bindings name expr => GBindings (K1 i name) (K1 i expr) where
  gbindings (K1 names) (K1 exprs) = fmap K1 <$> bindings names exprs


class Bindings name expr | name -> expr, expr -> name where
  bindings :: name -> expr -> State O.Tag (O.Bindings O.PrimExpr, expr)
  default bindings
    :: (GBindings (Rep name) (Rep expr), Generic name, Generic expr)
    => name -> expr -> State O.Tag (O.Bindings O.PrimExpr, expr)
  bindings names exprs =
    fmap to <$> gbindings (view generic names) (view generic exprs)


instance Bindings (Name a) (Expr a) where
  bindings (Name name) (Expr expr) =
    pure ([(symbol, expr)], Expr (O.AttrExpr symbol))
    where
      symbol = O.Symbol name Nothing


instance HigherKindedTable t => Bindings (t Name) (t Expr) where
  bindings = hbindings


instance Bindings name expr => Bindings (MaybeTable name) (MaybeTable expr) where
  bindings (MaybeTable _ names) (MaybeTable (Expr expr) exprs) = do
    symbol <- state (\tag -> (O.Symbol "maybe" (Just tag), O.next tag))
    (onames, oexprs) <- bindings names exprs
    pure ((symbol, expr) : onames, MaybeTable (Expr (O.AttrExpr symbol)) oexprs)


instance (Bindings name expr, Bindings name' expr') =>
  Bindings (TheseTable name name') (TheseTable expr expr')
 where
  bindings (TheseTable inames inames') (TheseTable iexprs iexprs') = do
    (onames, oexprs) <- bindings inames iexprs
    (onames', oexprs') <- bindings inames' iexprs'
    pure (onames <> onames', TheseTable oexprs oexprs')


instance (Bindings a a', Bindings b b') =>
  Bindings (a, b) (a', b')


instance (Bindings a a', Bindings b b', Bindings c c') =>
  Bindings (a, b, c) (a', b', c')


instance (Bindings a a', Bindings b b', Bindings c c', Bindings d d') =>
  Bindings (a, b, c, d) (a', b', c', d')


instance (Bindings a a', Bindings b b', Bindings c c', Bindings d d', Bindings e e') =>
  Bindings (a, b, c, d, e) (a', b', c', d', e')


renameColumns :: Bindings name expr => name -> O.QueryArr i expr -> O.QueryArr i expr
renameColumns names (O.QueryArr f) = O.QueryArr $ \(i, query, tag) ->
  let
    (exprs, query', tag') = f (i, query, tag)
    ((onames, oexprs), tag'') = runState (bindings names exprs) tag'
    query'' = O.Rebind False onames query'
  in
    (oexprs, query'', tag'')


class DefaultNames a where
  -- | Construct a record of 'Name's suitable for use with 'renameColumns' based on the
  -- field names of record selectors.
  defaultNames :: (String -> String) -> a
  default defaultNames :: (Generic a, GDefaultNames (Rep a)) => (String -> String) -> a
  defaultNames = to . gdefaultNames


instance HigherKindedTable t => DefaultNames (t Name) where
  defaultNames = hdefaultNames


instance DefaultNames a => DefaultNames (MaybeTable a) where
  defaultNames = pure . defaultNames


instance (DefaultNames a, DefaultNames b) => DefaultNames (TheseTable a b) where
  defaultNames f = TheseTable (defaultNames f) (defaultNames f)


instance (DefaultNames a, DefaultNames b) =>
  DefaultNames (a, b)


instance (DefaultNames a, DefaultNames b, DefaultNames c) =>
  DefaultNames (a, b, c)


instance (DefaultNames a, DefaultNames b, DefaultNames c, DefaultNames d) =>
  DefaultNames (a, b, c, d)


instance (DefaultNames a, DefaultNames b, DefaultNames c, DefaultNames d, DefaultNames e) =>
  DefaultNames (a, b, c, d, e)


class GDefaultNames a where
  gdefaultNames :: (String -> String) -> a x


instance {-# OVERLAPS #-} KnownSymbol name =>
  GDefaultNames (S1 ('MetaSel ('Just name) s u l) (K1 i (Name a)))
 where
  gdefaultNames f = M1 (K1 (Name (f (symbolVal (Proxy @name)))))


instance DefaultNames a => GDefaultNames (K1 i a) where
  gdefaultNames = K1 . defaultNames


instance GDefaultNames a => GDefaultNames (M1 i c a) where
  gdefaultNames = M1 . gdefaultNames


instance (GDefaultNames a, GDefaultNames b) => GDefaultNames (a :*: b) where
  gdefaultNames f = gdefaultNames f :*: gdefaultNames f
