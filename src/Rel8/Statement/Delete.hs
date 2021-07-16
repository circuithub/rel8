{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}

module Rel8.Statement.Delete
  ( Delete(..)
  , delete
  , ppDelete
  )
where

-- base
import Data.Kind ( Type )
import Prelude

-- hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Statement as Hasql

-- pretty
import Text.PrettyPrint ( Doc, (<+>), ($$), text )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Query ( Query )
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema, ppTable )
import Rel8.Statement.Returning ( Returning, decodeReturning, ppReturning )
import Rel8.Statement.Using ( ppUsing )
import Rel8.Statement.Where ( ppWhere )

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )


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


ppDelete :: Delete a -> Doc
ppDelete Delete {..} = case ppUsing using of
  Nothing ->
    text "DELETE FROM" <+> ppTable from $$
    text "WHERE false"
  Just (usingDoc, i) ->
    text "DELETE FROM" <+> ppTable from $$
    usingDoc $$
    ppWhere from (deleteWhere i) $$
    ppReturning from returning


-- | Run a 'Delete' statement.
delete :: Delete a -> Hasql.Statement () a
delete d@Delete {returning} = Hasql.Statement bytes params decode prepare
  where
    bytes = encodeUtf8 $ Text.pack sql
    params = Hasql.noParams
    decode = decodeReturning returning
    prepare = False
    sql = show doc
    doc = ppDelete d
