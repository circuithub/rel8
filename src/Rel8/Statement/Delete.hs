{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}
{-# language TypeApplications #-}

module Rel8.Statement.Delete
  ( Delete(..)
  , delete
  , delete_
  , ppDelete
  )
where

-- base
import Data.Kind ( Type )
import Data.Int (Int64)
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Statement as Hasql

-- opaleye
import qualified Opaleye.Internal.Tag as Opaleye

-- pretty
import Text.PrettyPrint ( Doc, (<+>), ($$), text )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Query ( Query )
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema, ppTable )
import Rel8.Statement.Returning
  ( Returning(Returning)
  , ppReturning
  )
import Rel8.Statement.Using ( ppUsing )
import Rel8.Statement.Where ( ppWhere )
import Rel8.Table.Serialize (Serializable, parse)

-- text
import qualified Data.Text as Text ( pack )
import Data.Text.Encoding ( encodeUtf8 )

-- transformers
import Control.Monad.Trans.State.Strict (State, evalState)


-- | The constituent parts of a @DELETE@ statement.
type Delete :: Type -> Type
data Delete a where
  Delete :: Selects names exprs =>
    { from :: TableSchema names
      -- ^ Which table to delete from.
    , using :: Query using
      -- ^ @USING@ clause — this can be used to join against other tables,
      -- and its results can be referenced in the @WHERE@ clause
    , deleteWhere :: using -> exprs -> Expr Bool
      -- ^ Which rows should be selected for deletion.
    , returning :: Returning names a
      -- ^ What to return from the @DELETE@ statement.
    }
    -> Delete a


ppDelete :: Delete a -> State Opaleye.Tag Doc
ppDelete Delete {..} = do
  musing <- ppUsing using
  pure $ case musing of
    Nothing ->
      text "DELETE FROM" <+> ppTable from $$
      text "WHERE false"
    Just (usingDoc, i) ->
      text "DELETE FROM" <+> ppTable from $$
      usingDoc $$
      ppWhere from (deleteWhere i) $$
      ppReturning from returning


-- | Run a @DELETE .. RETURNING@ statement.
delete :: Serializable expr a => Delete (Query expr) -> Hasql.Statement () [a]
delete d@Delete {returning} = Hasql.Statement bytes params decode prepare
  where
    bytes = encodeUtf8 $ Text.pack sql
    params = Hasql.noParams
    decode = case returning of
      Returning (_ :: exprs -> returning) -> Hasql.rowList (parse @returning)
    prepare = False
    sql = show doc
    doc = evalState (ppDelete d) Opaleye.start


-- | Run a @DELETE@ statement and return the number of rows affected.
delete_ :: Delete a -> Hasql.Statement () Int64
delete_ d = Hasql.Statement bytes params decode prepare
  where
    bytes = encodeUtf8 $ Text.pack sql
    params = Hasql.noParams
    decode = Hasql.rowsAffected
    prepare = False
    sql = show doc
    doc = evalState (ppDelete d) Opaleye.start
