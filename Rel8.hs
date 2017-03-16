{-# LANGUAGE Arrows #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Rel8
  ( -- * Defining Tables
    C
  , Anon
  , HasDefault(..)
  , BaseTable(tableName)
  , Table

    -- * Querying Tables
  , O.Query, O.QueryArr
  , queryTable
  , leftJoin
  , leftJoinA
  , unionAll
  , O.exists, O.notExists

    -- ** Filtering
  , where_
  , filterQuery
  , distinct

    -- ** Offset and limit
  , O.limit
  , O.offset

    -- ** Ordering
  , asc, desc, orderNulls, O.orderBy, OrderNulls(..)

    -- * Aggregation
  , aggregate
  , AggregateTable
  , count, groupBy, DBSum(..), countStar, DBMin(..), DBMax(..), DBAvg(..)
  , boolAnd, boolOr, stringAgg, arrayAgg, countDistinct
  , countRows, Aggregate

    -- * Tables
  , MaybeTable, isTableNull
  , Col(..)

    -- * Expressions
  , Expr, coerceExpr, dbShow, case_

    -- ** Equality
  , DBEq, (==.), in_, ilike

    -- ** Ordering
  , DBOrd, (>.), (>=.), (<.), (<=.)

    -- ** Numeric Operators
  , (+), (-), negate, (*)

    -- ** Booleans
  , (&&.), (||.), not_, Predicate

    -- ** Literals
  , DBType(..), lit, dbNow
  , TypeInfo(..), showableDbType, compositeDBType

    -- ** Null
  , ToNullable(toNullable) , ($?), isNull, nullable
  , liftOpNull, mapNull

    -- *** Null-lifted operators
  , (==?), (<?), (<=?), (>?), (>=?)
  , (||?), (&&?), (+?), (*?), (-?)
  , (/?)

    -- * Running Queries
    -- ** @SELECT@
  , select

    -- ** @INSERT@
  , Default(..), Insert
  , insert, insert1Returning {- , insertReturning -}

    -- ** @UPDATE@
  , update {- , updateReturning -}

    -- ** @DELETE@
  , delete

    -- * Interpretations
  , QueryResult, SchemaInfo

    -- * Re-exported symbols
  , Connection, Stream, Of, Generic

    -- * Unsafe routines
  , unsafeCoerceExpr
  , unsafeCastExpr
  , unsafeLiteral
  , dbFunction
  , nullaryFunction
  , dbBinOp
  ) where

import Data.Functor.Rep (mzipWithRep)
import Control.Applicative (liftA2)
import Control.Category ((.), id)
import Control.Lens (view, from)
import Control.Monad.Rel8
import Data.List (foldl')
import Data.Profunctor (lmap)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import qualified Opaleye.Binary as O
import qualified Opaleye.Column as O
import qualified Opaleye.Internal.Aggregate as O
import qualified Opaleye.Internal.Binary as O
import qualified Opaleye.Internal.Column as O
import qualified Opaleye.Internal.Distinct as O
import qualified Opaleye.Internal.HaskellDB.PrimQuery as O
import qualified Opaleye.Internal.Join as O
import qualified Opaleye.Internal.PackMap as O
import qualified Opaleye.Internal.PGTypes as O
import qualified Opaleye.Internal.PrimQuery as PrimQuery
import qualified Opaleye.Internal.QueryArr as O
import qualified Opaleye.Internal.Unpackspec as O
import qualified Opaleye.Join as O
import qualified Opaleye.Operators as O
import qualified Opaleye.Order as O
import Prelude hiding (not, (.), id)
import Rel8.Internal
import Streaming (Of, Stream)

infix 4 ==? , <? , <=? , >? , >=?
infixr 2 ||?,  &&?
infixl 7 *?
infixl 6 +?, -?

--------------------------------------------------------------------------------
(==?)
  :: (DBEq a, ToNullable l (Maybe a), ToNullable r (Maybe a))
  => Expr l -> Expr r -> Expr (Maybe Bool)
a ==? b = liftOpNull (==.) (toNullable a) (toNullable b)

(<?), (<=?), (>?), (>=?)
  :: (DBOrd a, ToNullable l (Maybe a), ToNullable r (Maybe a))
  => Expr l -> Expr r -> Expr (Maybe Bool)
a <? b = liftOpNull (<.) (toNullable a) (toNullable b)
a <=? b = liftOpNull (<=.) (toNullable a) (toNullable b)
a >? b = liftOpNull (>.) (toNullable a) (toNullable b)
a >=? b = liftOpNull (>=.) (toNullable a) (toNullable b)

(||?), (&&?)
  :: (ToNullable bool1 (Maybe Bool), ToNullable bool2 (Maybe Bool))
  => Expr bool1 -> Expr bool2 -> Expr (Maybe Bool)
a ||? b = liftOpNull (||.) (toNullable a) (toNullable b)
a &&? b = liftOpNull (&&.) (toNullable a) (toNullable b)

(+?), (*?), (-?)
  :: (Num (Expr a), ToNullable l (Maybe a), ToNullable r (Maybe a))
  => Expr l -> Expr r -> Expr (Maybe a)
a +? b = liftOpNull (+) (toNullable a) (toNullable b)
a *? b = liftOpNull (*) (toNullable a) (toNullable b)
a -? b = liftOpNull (-) (toNullable a) (toNullable b)

(/?)
  :: (Fractional (Expr a), ToNullable l (Maybe a), ToNullable r (Maybe a))
  => Expr l -> Expr r -> Expr (Maybe a)
a /? b = liftOpNull (/) (toNullable a) (toNullable b)

--------------------------------------------------------------------------------
unsafeLiteral :: String -> Expr a
unsafeLiteral = columnToExpr . O.literalColumn . O.OtherLit

--------------------------------------------------------------------------------
-- | Take the @LEFT JOIN@ of two queries.
leftJoin
  :: (Table left a, Table right b, Predicate bool)
  => (left -> right -> Expr bool) -- ^ The condition to join upon.
  -> O.Query left -- ^ The left table
  -> O.Query right -- ^ The right table
  -> O.Query (left,MaybeTable right)
leftJoin condition l r =
  O.leftJoinExplicit
    unpackColumns
    unpackColumns
    (O.NullMaker (\(tag, t) -> MaybeTable tag t))
    l
    (liftA2 (,) (pure (lit (Just False))) r)
    (\(a, (_, b)) -> exprToColumn (toNullable (condition a b)))


--------------------------------------------------------------------------------
-- | A more convenient form of 'leftJoin' when using arrow notation.
-- @inlineLeftJoinA@ takes the left join of all proceeding queries against a
-- given query. The input to the 'QueryArr' is a predicate function against
-- rows in the to-be-joined query.
--
-- === Example
-- @
-- -- Return all users and comments, including users who haven't made a comment.
-- usersAndComments :: Query (User Expr, MaybeTable (Comment Expr))
-- proc _ -> do
--   u <- queryTable -< ()
--   comment <- inlineLeftJoinA -< \c -> commentUser c ==. userId u
--   returnA (u, c)
-- @
leftJoinA
  :: (Table a haskell, Predicate bool)
  => O.Query a -> O.QueryArr (a -> Expr bool) (MaybeTable a)
leftJoinA q =
  O.QueryArr $ \(p, left, t) ->
    let O.QueryArr rightQueryF = liftA2 (,) (pure (lit (Just False))) q
        (right, pqR, t') = rightQueryF ((), PrimQuery.Unit, t)
        ((tag, renamed), ljPEsB) =
          O.run
            (O.runUnpackspec unpackColumns (O.extractLeftJoinFields 2 t') right)
    in ( MaybeTable tag renamed
       , PrimQuery.Join
           PrimQuery.LeftJoin
           (case toNullable (p renamed) of
              Expr a -> a)
           [] -- TODO ?
           ljPEsB
           left
           pqR
       , t')

-- | Take only distinct rows in a 'O.Query'. This maps to grouping by every
-- column in the table.
distinct :: Table table haskell => O.Query table -> O.Query table
distinct =
  O.distinctExplicit
    (O.Distinctspec
       (O.Aggregator (O.PackMap (\f -> traversePrimExprs (\e -> f (Nothing,e))))))

-- | Restrict a 'O.QueryArr' to only contain rows that satisfy a given predicate.
where_ :: Predicate bool => O.QueryArr (Expr bool) ()
where_ = lmap (exprToColumn . toNullable) O.restrict

-- | Filter a 'O.Query' into a new query where all rows satisfy a given
-- predicate.
filterQuery :: Predicate bool => (a -> Expr bool) -> O.Query a -> O.Query a
filterQuery f q = proc _ -> do
  row <- q -< ()
  where_ -< f row
  id -< row

-- | Corresponds to the @IS NULL@ operator.
isNull :: Expr (Maybe a) -> Expr Bool
isNull = columnToExpr . O.isNull . exprToColumn

-- | Test if an 'Expr' is in a list of 'Expr's. This is performed by folding
-- '==.' over all values and combining them with '||.'.
in_ :: DBEq a => Expr a -> [Expr a] -> Expr Bool
in_ x = foldl' (\b y -> x ==. y ||. b) (lit False)

-- | Corresponds to the @ILIKE@ operator.
ilike :: Expr Text -> Expr Text -> Expr Bool
a `ilike` b =
  columnToExpr (O.binOp (O.OpOther "ILIKE") (exprToColumn a) (exprToColumn b))


dbBinOp :: String -> Expr a -> Expr b -> Expr c
dbBinOp op a b =
  columnToExpr (O.binOp (O.OpOther op) (exprToColumn a) (exprToColumn b))

-- | Corresponds to the @now()@ function.
dbNow :: Expr UTCTime
dbNow = nullaryFunction "now"

-- | Take the union of all rows in the first query and all rows in the second
-- query. Corresponds to the PostgreSQL @UNION ALL@ operator.
unionAll :: Table table haskell => O.Query table -> O.Query table -> O.Query table
unionAll =
  O.unionAllExplicit
    (O.Binaryspec
       (O.PackMap
          (\f (l, r) ->
             fmap
               (view (from expressions))
               (sequenceA
                  ((mzipWithRep
                      (\prim1 prim2 -> f (prim1, prim2))
                      (view expressions l)
                      (view expressions r)))))))
