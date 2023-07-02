{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language ViewPatterns #-}

module Rel8.Statement
  ( Statement (..)
  , IsStatement
  , run
  , toStatement
  , showStatement
  )
where

-- base
import Data.Kind (Type, Constraint)
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Statement as Hasql

-- opaleye
import qualified Opaleye.Internal.Tag as Opaleye

-- pretty
import Text.PrettyPrint (Doc)

-- rel8
import Rel8.Expr (Expr)
import Rel8.Query (Query)
import Rel8.Statement.Delete (Delete, decodeDelete, ppDelete, withDelete)
import Rel8.Statement.Insert (Insert, decodeInsert, ppInsert, withInsert)
import Rel8.Statement.Rows (Rows (..))
import Rel8.Statement.Select (decodeSelect, ppSelect)
import Rel8.Statement.Update (Update, decodeUpdate, ppUpdate, withUpdate)
import Rel8.Statement.With (With, applyWith, justWith, ppAlias, ppWith)
import Rel8.Table (Table)

-- text
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)

-- transformers
import Control.Monad.Trans.State.Strict (State, evalState)


-- | In addition to @SELECT@, @INSERT@, @UPDATE@ and @DELETE@, PostgreSQL
-- also supports compositions thereof via its statement-level @WITH@ syntax
-- (with some caveats). Each such \"sub-statement\" can reference the results
-- of previous sub-statements. This pseudo-monadic interface is encapsulated
-- by the 'Rel8.Statement.Do.>>=' from "Rel8.Statement.Do", which can be
-- invoked using the @QualifiedDo@ language extension.
--
-- The caveat with this is that the [side-effects of these sub-statements
-- are not visible to other sub-statements](https://www.postgresql.org/docs/current/queries-with.html#QUERIES-WITH-MODIFYING),
-- only the results of a @SELECT@ or @RETURNING@ clause. So an @INSERT@ into
-- a table followed immediately by a select therefrom will not include the
-- inserted rows; however, it is possible to return the inserted rows using
-- @RETURNING@, and to 'Rel8.unionAll' this with the result of a @SELECT@
-- from the same table, which will produce the desired result.
--
-- An example of where this can be useful is if you want to delete rows from
-- a table and to immediately log their deletion in a log table.
--
-- @
-- import qualified Rel8.Statement.Do as Statement
--
-- deleteFoo :: (Foo Expr -> Expr Bool) -> Statement 'Nothing
-- deleteFoo predicate = Statement.do
--   foos <-
--     Delete
--       { from = fooSchema
--       , using = pure ()
--       , deleteWhere = \_ -> predicate
--       , returning = Returning id
--       }
--   Insert
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
type Statement :: Maybe Type -> Type
data Statement returning where
  Select :: Table Expr a => Query a -> Statement ('Just a)
  Insert :: Insert a -> Statement a
  Update :: Update a -> Statement a
  Delete :: Delete a -> Statement a
  Bind :: Statement ('Just a) -> (Query a -> Statement b) -> Statement b
  Then :: Statement a -> Statement b -> Statement b


ppDecodeStatement :: Statement exprs -> (Doc, Rows exprs a -> Hasql.Result a)
ppDecodeStatement = \statement -> (`evalState` Opaleye.start) $ do
  (dlist, _, _, doc, decoder) <- go id statement
  let
    withs = dlist []
    doc' = ppWith withs doc 
  pure (doc', decoder)
  where
    go :: ()
      => ([(Doc, Doc)] -> [(Doc, Doc)])
      -> Statement exprs
      -> State Opaleye.Tag
           ( [(Doc, Doc)] -> [(Doc, Doc)]
           , [(Doc, Doc)] -> [(Doc, Doc)]
           , With exprs
           , Doc
           , Rows exprs a -> Hasql.Result a
           )
    go dlist = \case
      Select @exprs query -> do
        doc <- ppSelect query
        with <- justWith @exprs
        let
          dlist' = dlist . ((ppAlias with, doc) :)
          decode rows = decodeSelect rows
        pure (dlist, dlist', with, doc, decode)
      Insert insert -> do
        doc <- ppInsert insert
        with <- withInsert insert
        let
          dlist' = dlist . ((ppAlias with, doc) :)
          decode rows = decodeInsert rows insert
        pure (dlist, dlist', with, doc, decode)
      Update update -> do
        doc <- ppUpdate update
        with <- withUpdate update
        let
          dlist' = dlist . ((ppAlias with, doc) :)
          decode rows = decodeUpdate rows update
        pure (dlist, dlist', with, doc, decode)
      Delete delete -> do
        doc <- ppDelete delete
        with <- withDelete delete
        let
          dlist' = dlist . ((ppAlias with, doc) :)
          decode rows = decodeDelete rows delete
        pure (dlist, dlist', with, doc, decode)
      Bind prev f -> do
        (_, dlist', with, _, _) <- go dlist prev
        let
          next = applyWith with f
        go dlist' next
      Then prev next -> do
        (_, dlist', _, _, _) <- go dlist prev
        go dlist' next


-- | 'IsStatement' allows 'run' to be polymorphic in the types of statements
-- it accepts. It can be either 'Query', 'Insert', 'Update', 'Delete' or
-- 'Statement'.
type IsStatement :: Maybe Type -> Type -> Constraint
class IsStatement returning statement | statement -> returning where
  toStatement :: statement -> Statement returning


instance (f ~ Query, Table Expr a) => IsStatement ('Just a) (f a) where
  toStatement = Select


instance IsStatement a (Insert a) where
  toStatement = Insert


instance IsStatement a (Update a) where
  toStatement = Update


instance IsStatement a (Delete a) where
  toStatement = Delete


instance IsStatement a (Statement a) where
  toStatement = id


-- | Convert an 'IsStatement' (e.g., a 'Query' or an 'Insert') to a runnable
-- 'Hasql.Statement' returning rows according to the given 'Rows' parameter.
run :: IsStatement exprs statement
  => Rows exprs a -> statement -> Hasql.Statement () a
run rows statement =
  Hasql.Statement bytes params (decode rows) prepare
  where
    bytes = encodeUtf8 $ Text.pack sql
    params = Hasql.noParams
    prepare = False
    sql = show doc
    (doc, decode) = ppDecodeStatement (toStatement statement)


-- | Convert a 'Statement' to a 'String' containing a @SELECT@, @INSERT@,
-- @UPDATE@, @DELETE@ or @WITH@ statement.
showStatement :: Statement a -> String
showStatement = show . fst . ppDecodeStatement