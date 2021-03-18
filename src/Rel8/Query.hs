{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Query
  ( Query
  , liftOpaleye
  , toOpaleye
  , selectQuery
  , countRows
  , select
  , where_
  , whereExists
  , whereNotExists
  , exists
  , each
  , union
  , unionAll
  , intersect
  , intersectAll
  , except
  , exceptAll
  , distinct
  , distinctOn
  , limit
  , offset
  , values
  , filter
  , mapOpaleye
  , showQuery
  ) where

-- base
import Control.Exception ( throwIO )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Foldable ( fold, toList )
import Data.Int ( Int64 )
import Numeric.Natural ( Natural )
import Prelude
  ( Applicative
  , Bool( False , True )
  , Foldable
  , Functor( fmap )
  , Maybe( Just, Nothing )
  , Monad( return, (>>=) )
  , String
  , ($)
  , (.)
  , (<$)
  , either
  , fromIntegral
  )

-- hasql
import Hasql.Connection ( Connection )
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- profunctors
import Data.Profunctor ( lmap )

-- rel8
import qualified Opaleye ( valuesExplicit )
import qualified Opaleye.Aggregate as Opaleye
import qualified Opaleye.Binary as Opaleye
import qualified Opaleye.Distinct as Opaleye
import qualified Opaleye.Exists as Opaleye
import qualified Opaleye.Internal.Optimize as Opaleye
import qualified Opaleye.Internal.Order as Opaleye
import qualified Opaleye.Internal.Print as Opaleye ( formatAndShowSQL )
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Operators as Opaleye hiding ( exists )
import qualified Opaleye.Order as Opaleye ( limit, offset )
import qualified Opaleye.Table as Opaleye
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( columnToExpr, exprToColumn )
import qualified Rel8.Optimize
import Rel8.Serializable ( Serializable, hasqlRowDecoder )
import Rel8.Table ( Table )
import Rel8.Table.Opaleye ( binaryspec, distinctspec, unpackspec, valuesspec )
import Rel8.Table.Selects ( Selects )
import Rel8.TableSchema ( TableSchema, selectSchema )

-- text
import Data.Text ( pack )
import Data.Text.Encoding ( encodeUtf8 )


-- | The type of @SELECT@able queries. You generally will not explicitly use
-- this type, instead preferring to be polymorphic over any 'MonadQuery m'.
-- Functions like 'select' will instantiate @m@ to be 'Query' when they run
-- queries.
newtype Query a = Query (Opaleye.Query a)
  deriving newtype (Functor, Applicative, Monad)


liftOpaleye :: Opaleye.Query a -> Query a
liftOpaleye = Query


toOpaleye :: Query a -> Opaleye.Query a
toOpaleye (Query q) = q


mapOpaleye :: (Opaleye.Query a -> Opaleye.Query b) -> Query a -> Query b
mapOpaleye f = liftOpaleye . f . toOpaleye


-- | Run a @SELECT@ query, returning all rows.
select :: forall row haskell m. (Serializable row haskell, MonadIO m) => Connection -> Query row -> m [haskell]
select conn query = liftIO case selectQuery query of
  Nothing -> return []
  Just neQuery ->
    Hasql.run session conn >>= either throwIO return
    where
      session = Hasql.statement () statement
      statement = Hasql.Statement q params (Hasql.rowList (hasqlRowDecoder @row)) prepare
      q = encodeUtf8 (pack neQuery)
      params = Hasql.noParams
      prepare = False


selectQuery :: forall a . Table Expr a => Query a -> Maybe String
selectQuery (Query opaleye) = showSqlForPostgresExplicit
  where
    showSqlForPostgresExplicit =
      case Opaleye.runQueryArrUnpack unpackspec opaleye of
        (x, y, z) -> Opaleye.formatAndShowSQL True (x , Rel8.Optimize.optimize (Opaleye.optimize y) , z)


-- | Count the occurances of a single column. Corresponds to @COUNT(a)@
countRows :: Query a -> Query (Expr Int64)
countRows = fmap columnToExpr . mapOpaleye Opaleye.countRows


-- | Drop any rows that don't match a predicate.  @where_ expr@ is equivalent
-- to the SQL @WHERE expr@.
--
-- >>> :{
-- select c $ do
--   x <- values [ lit x | x <- [ 1..5 :: Int32 ] ]
--   where_ $ x >. lit 2
--   return x
-- :}
-- [3,4,5]
where_ :: Expr Bool -> Query ()
where_ x = liftOpaleye $ lmap (\_ -> exprToColumn x) Opaleye.restrict


-- | Produce the empty query if the given query returns no rows. @whereExists@
-- is equivalent to @WHERE EXISTS@ in SQL.
--
-- >>> :{
-- select c do
--   author <- each authorSchema
--   whereExists do
--     project <- each projectSchema
--     where_ $ projectAuthorId project ==. authorId author
--   return $ authorName author
-- :}
-- ["Ollie","Bryan O'Sullivan"]
whereExists :: Query a -> Query ()
whereExists = mapOpaleye Opaleye.restrictExists


-- | Produce the empty query if the given query returns rows. @whereNotExists@
-- is equivalent to @WHERE NOT EXISTS@ in SQL.
--
-- >>> :{
-- select c do
--   author <- each authorSchema
--   whereNotExists do
--     project <- each projectSchema
--     where_ $ projectAuthorId project ==. authorId author
--   return $ authorName author
-- :}
-- ["Emily Pillmore"]
whereNotExists :: Query a -> Query ()
whereNotExists = mapOpaleye Opaleye.restrictNotExists


-- | Checks if a query returns at least one row.
--
-- >>> :{
-- mapM_ print =<< select c do
--   author <- each authorSchema
--   hasProjects <- exists do
--     project <- each projectSchema
--     where_ $ authorId author ==. projectAuthorId project
--   return (authorName author, hasProjects)
-- :}
-- ("Ollie",True)
-- ("Bryan O'Sullivan",True)
-- ("Emily Pillmore",False)
exists :: Query a -> Query (Expr Bool)
exists = fmap columnToExpr . mapOpaleye Opaleye.exists


-- | Select each row from a table definition. This is equivalent to @FROM
-- table@.
--
-- >>> mapM_ print =<< select c (each projectSchema)
-- Project {projectAuthorId = 1, projectName = "rel8"}
-- Project {projectAuthorId = 2, projectName = "aeson"}
-- Project {projectAuthorId = 2, projectName = "text"}
each :: Selects schema row => TableSchema schema -> Query row
each = liftOpaleye . Opaleye.selectTableExplicit unpackspec . selectSchema


-- | Combine the results of two queries of the same type, collapsing
-- duplicates.  @union a b@ is the same as the SQL statement @x UNION b@.
--
-- >>> select c $ values [lit True, lit True, lit False] `union` values [lit True]
-- [False,True]
union :: Table Expr a => Query a -> Query a -> Query a
union l r = liftOpaleye $ Opaleye.unionExplicit binaryspec (toOpaleye l) (toOpaleye r)


-- | Combine the results of two queries of the same type, retaining duplicates.
-- @unionAll a b@ is the same as the SQL statement @x UNION ALL b@.
--
-- >>> select c $ values [lit True, lit True, lit False] `unionAll` values [lit True]
-- [True,True,False,True]
unionAll :: Table Expr a => Query a -> Query a -> Query a
unionAll l r = liftOpaleye $ Opaleye.unionAllExplicit binaryspec (toOpaleye l) (toOpaleye r)


-- | Find the intersection of two queries, collapsing duplicates.  @intersect a
-- b@ is the same as the SQL statement @x INTERSECT b@.
--
-- >>> select c $ values [lit True, lit True, lit False] `intersect` values [lit True]
-- [True]
intersect :: Table Expr a => Query a -> Query a -> Query a
intersect l r = liftOpaleye $ Opaleye.intersectExplicit binaryspec (toOpaleye l) (toOpaleye r)


-- | Find the intersection of two queries, retaining duplicates.  @intersectAll
-- a b@ is the same as the SQL statement @x INTERSECT ALL b@.
--
-- >>> select c $ values [lit True, lit True, lit False] `intersectAll` values [lit True, lit True]
-- [True,True]
intersectAll :: Table Expr a => Query a -> Query a -> Query a
intersectAll l r = liftOpaleye $ Opaleye.intersectAllExplicit binaryspec (toOpaleye l) (toOpaleye r)


-- | Find the difference of two queries, collapsing duplicates @except a b@ is
-- the same as the SQL statement @x INTERSECT b@.
--
-- >>> select c $ values [lit True, lit False, lit False] `except` values [lit True]
-- [False]
except :: Table Expr a => Query a -> Query a -> Query a
except l r = liftOpaleye $ Opaleye.exceptExplicit binaryspec (toOpaleye l) (toOpaleye r)


-- | Find the difference of two queries, retaining duplicates.  @exceptAll a b@
-- is the same as the SQL statement @x EXCEPT ALL b@.
--
-- >>> select c $ values [lit True, lit False, lit False] `exceptAll` values [lit True]
-- [False,False]
exceptAll :: Table Expr a => Query a -> Query a -> Query a
exceptAll l r = liftOpaleye $ Opaleye.exceptAllExplicit binaryspec (toOpaleye l) (toOpaleye r)


-- | Select all distinct rows from a query, removing duplicates.  @distinct q@
-- is equivalent to the SQL statement @SELECT DISTINCT q@.
--
-- >>> select c $ distinct $ values [ lit True, lit True, lit False ]
-- [False,True]
distinct :: Table Expr a => Query a -> Query a
distinct = mapOpaleye (Opaleye.distinctExplicit distinctspec)


distinctOn :: Table Expr b => (a -> b) -> Query a -> Query a
distinctOn proj =
  mapOpaleye (\q -> Opaleye.productQueryArr (Opaleye.distinctOn unpackspec proj . Opaleye.runSimpleQueryArr q))


-- | @limit n@ select at most @n@ rows from a query.  @limit n@ is equivalent
-- to the SQL @LIMIT n@.
--
-- >>> select c $ limit 3 $ values [ lit x | x <- [ 1..5 :: Int32 ] ]
-- [1,2,3]
limit :: Natural -> Query a -> Query a
limit n = mapOpaleye $ Opaleye.limit (fromIntegral n)


-- | @offset n@ drops the first @n@ rows from a query. @offset n@ is equivalent
-- to the SQL @OFFSET n@.
--
-- >>> select c $ offset 3 $ values [ lit x | x <- [ 1..5 :: Int32 ] ]
-- [4,5]
offset :: Natural -> Query a -> Query a
offset n = mapOpaleye $ Opaleye.offset (fromIntegral n)


-- | Construct a query that returns the given input list of rows. This is like
-- folding a list of 'return' statements under 'union', but uses the SQL
-- @VALUES@ expression for efficiency.
--
-- Typically @values@ will be used with 'lit':
--
-- >>> mapM_ Data.Text.IO.putStrLn =<< select c (values [ lit "Hello", lit "World!" ])
-- Hello
-- World!
values :: forall expr f. (Table Expr expr, Foldable f) => f expr -> Query expr
values = liftOpaleye . Opaleye.valuesExplicit valuesspec . toList


-- | @filter f x@ will be a zero-row query when @f x@ is @False@, and will
-- return @x@ unchanged when @f x@ is @True@. This is similar to
-- 'Control.Monad.guard', but as the predicate is separate from the argument,
-- it is easy to use in a pipeline of 'Query' transformations.
--
-- >>> select c $ values [ lit x | x <- [ 1..5 :: Int32 ] ] >>= filter (>. 3)
-- [4,5]
filter :: (a -> Expr Bool) -> a -> Query a
filter f a = a <$ where_ (f a)


-- | Convert a query to a 'String' containing the query as a @SELECT@
-- statement.
showQuery :: Table Expr a => Query a -> String
showQuery = fold . selectQuery
