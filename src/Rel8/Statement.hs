{-# language DeriveFunctor #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

module Rel8.Statement
  ( Statement
  , statementReturning
  , statementNoReturning
  , ppDecodeStatement
  )
where

-- base
import Control.Applicative (liftA2)
import Control.Monad (ap, liftM2)
import Data.Foldable (fold, toList)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty, intersperse)
import Data.Monoid (Endo (Endo))
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.Tag as Opaleye

-- pretty
import Text.PrettyPrint
  ( Doc
  , (<+>)
  , ($$)
  , comma
  , doubleQuotes
  , hcat
  , parens
  , punctuate
  , text
  , vcat
  )

-- rel8
import Rel8.Expr (Expr)
import Rel8.Expr.Bool (false)
import Rel8.Query (Query)
import Rel8.Query.Aggregate (countRows)
import Rel8.Query.Each (each)
import Rel8.Schema.Table (TableSchema (..))
import Rel8.Statement.Rows (Rows (..))
import Rel8.Table (Table)
import Rel8.Table.Cols (fromCols)
import Rel8.Table.Name (namesFromLabelsWithA, showNames)
import Rel8.Table.Serialize (parse)

-- semigroupoids
import Data.Functor.Apply (Apply, WrappedApplicative (..))
import Data.Functor.Bind (Bind, (>>-))

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (State, evalState)
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT, tell)


type Binding :: Type
data Binding = Binding
  { relation :: !String
  , columns :: !(Maybe (NonEmpty String))
  , doc :: !Doc
  , returning :: !Returning
  }


type Result :: Type -> Type
data Result a = Unmodified !a | Modified !a


instance Functor Result where
  fmap f = \case
    Unmodified a -> Modified (f a)
    Modified a -> Modified (f a)


getResult :: Result a -> a
getResult = \case
  Unmodified a -> a
  Modified a -> a


type Returning :: Type
data Returning where
  NoReturning :: Returning
  Returning :: Query (Expr Int64) -> Returning 


-- | 'Statement' represents a single PostgreSQL statement. Most commonly,
-- this is constructed using 'Rel8.select', 'Rel8.insert', 'Rel8.update'
-- or 'Rel8.delete'.
--
-- However, in addition to @SELECT@, @INSERT@, @UPDATE@ and @DELETE@,
-- PostgreSQL also supports compositions thereof via its statement-level
-- @WITH@ syntax (with some caveats). Each such \"sub-statement\" can
-- reference the results of previous sub-statements. 'Statement' provides a
-- 'Monad' instance that captures this \"binding\" pattern.
--
-- The caveat with this is that the [side-effects of these sub-statements
-- are not visible to other sub-statements](https://www.postgresql.org/docs/current/queries-with.html#QUERIES-WITH-MODIFYING);
-- only the explicit results of previous sub-statements (from @SELECT@s or
-- @RETURNING@ clauses) are visible. So, for example, an @INSERT@ into a table
-- followed immediately by a @SELECT@ therefrom will not return the inserted
-- rows. However, it is possible to return the inserted rows using
-- @RETURNING@, 'Rel8.unionAll'ing this with the result of a @SELECT@
-- from the same table will produce the desired result.
--
-- An example of where this can be useful is if you want to delete rows from
-- a table and simultaneously log their deletion in a log table.
--
-- @
-- deleteFoo :: (Foo Expr -> Expr Bool) -> Statement ()
-- deleteFoo predicate = do
--   foos <-
--     delete Delete
--       { from = fooSchema
--       , using = pure ()
--       , deleteWhere = \_ -> predicate
--       , returning = Returning id
--       }
--   insert Insert
--     { into = deletedFooSchema
--     , rows = do
--         Foo {..} <- foos
--         let
--           deletedAt = 'Rel8.Expr.Time.now'
--         pure DeletedFoo {..}
--     , onConflict = Abort
--     , returning = NoReturning
--     }
-- @
newtype Statement a =
  Statement (WriterT (Endo [Binding]) (State Opaleye.Tag) (Result a))
  deriving stock (Functor)
  deriving (Apply) via WrappedApplicative Statement


instance Applicative Statement where
  pure = Statement . pure . Modified
  (<*>) = ap
  liftA2 = liftM2


instance Bind Statement where
  Statement m >>- f = Statement $ do
    result <- m
    case f (getResult result) of
      Statement m' -> m'


instance Monad Statement where
  (>>=) = (>>-)


statementNoReturning :: State Opaleye.Tag Doc -> Statement ()
statementNoReturning pp = Statement $ do
  binding <- lift $ do
    doc <- pp
    tag <- Opaleye.fresh
    let
      relation = Opaleye.tagWith tag "statement"
      columns = Nothing
      returning = NoReturning
      binding = Binding {..}
    pure binding
  tell (Endo (binding :))
  pure $ Unmodified ()


statementReturning :: Table Expr a 
  => State Opaleye.Tag Doc -> Statement (Query a)
statementReturning pp = Statement $ do
  (binding, query) <- lift $ do
    doc <- pp
    tag <- Opaleye.fresh
    let
      relation = Opaleye.tagWith tag "statement"
      symbol labels = do
        subtag <- Opaleye.fresh
        let
          suffix = Opaleye.tagWith tag (Opaleye.tagWith subtag "")
        pure $ take (63 - length suffix) label ++ suffix
        where
          label = fold (intersperse "/" labels)
      names = namesFromLabelsWithA symbol `evalState` Opaleye.start
      columns = Just $ showNames names
      query =
        fromCols <$> each
          TableSchema
            { name = relation
            , schema = Nothing
            , columns = names
            }
      returning = Returning (countRows query)
      binding = Binding {..}
    pure (binding, query)
  tell (Endo (binding :))
  pure $ Unmodified query


ppDecodeStatement :: ()
  => (forall x. Table Expr x => Query x -> State Opaleye.Tag Doc)
  -> Rows exprs a -> Statement exprs -> (Doc, Hasql.Result a)
ppDecodeStatement ppSelect rows (Statement m) = evalState go Opaleye.start
  where
    go = do
      (result, Endo dlist) <- runWriterT m
      let
        bindings' = dlist []
      case unsnoc bindings' of
        Nothing -> case rows of
          Void -> do
            doc <- ppSelect (pure false)
            pure (doc, Hasql.noResult)
          RowsAffected -> do
            doc <- ppSelect (pure false)
            pure (doc, 0 <$ Hasql.noResult)
          Single @exprs @a -> do
            doc <- ppSelect (getResult result)
            pure (doc, Hasql.singleRow (parse @exprs @a))
          Maybe @exprs @a -> do
            doc <- ppSelect (getResult result)
            pure (doc, Hasql.rowMaybe (parse @exprs @a))
          List @exprs @a -> do
            doc <- ppSelect (getResult result)
            pure (doc, Hasql.rowList (parse @exprs @a))
          Vector @exprs @a -> do
            doc <- ppSelect (getResult result)
            pure (doc, Hasql.rowVector (parse @exprs @a))
        Just (bindings, binding@Binding {doc = after}) -> case rows of
          Void -> pure (doc, Hasql.noResult)
            where
              doc = ppWith bindings after
          RowsAffected -> do
            case result of
              Unmodified _ -> pure (doc, Hasql.rowsAffected)
                where
                  doc = ppWith bindings after
              Modified _ -> case returning binding of
                NoReturning -> pure (doc, Hasql.rowsAffected)
                  where
                    doc = ppWith bindings after 
                Returning query -> do
                  doc <- ppWith bindings' <$> ppSelect query
                  pure (doc, Hasql.singleRow parse)
          Single @exprs @a -> do
            case result of
              Unmodified _ -> pure (doc, Hasql.singleRow (parse @exprs @a))
                where
                  doc = ppWith bindings after
              Modified query -> do
                doc <- ppWith bindings' <$> ppSelect query
                pure (doc, Hasql.singleRow (parse @exprs @a))
          Maybe @exprs @a -> do
            case result of
              Unmodified _ -> pure (doc, Hasql.rowMaybe (parse @exprs @a))
                where
                  doc = ppWith bindings after
              Modified query -> do
                doc <- ppWith bindings' <$> ppSelect query
                pure (doc, Hasql.rowMaybe (parse @exprs @a))
          List @exprs @a -> do
            case result of
              Unmodified _ -> pure (doc, Hasql.rowList (parse @exprs @a))
                where
                  doc = ppWith bindings after
              Modified query -> do
                doc <- ppWith bindings' <$> ppSelect query
                pure (doc, Hasql.rowList (parse @exprs @a))
          Vector @exprs @a -> do
            case result of
              Unmodified _ -> pure (doc, Hasql.rowVector (parse @exprs @a))
                where
                  doc = ppWith bindings after
              Modified query -> do
                doc <- ppWith bindings' <$> ppSelect query
                pure (doc, Hasql.rowVector (parse @exprs @a))


ppWith :: [Binding] -> Doc -> Doc
ppWith bindings after = pre $$ after
  where
    pre = case bindings of
      [] -> mempty
      _ ->
        text "WITH" <+>
        vcat (punctuate comma (map go bindings))
    go binding@Binding {doc = before} =
      ppAlias binding $$
      text "AS" <+>
      parens before


ppAlias :: Binding -> Doc
ppAlias Binding {relation, columns = mcolumns} = case mcolumns of
  Nothing -> escape relation
  Just columns -> 
    escape relation <+>
    parens (hcat (punctuate comma (escape <$> toList columns)))


escape :: String -> Doc
escape = doubleQuotes . text . concatMap go
  where
    go = \case
      '"' -> "\"\""
      c -> [c]


unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
