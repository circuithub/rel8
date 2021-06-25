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
import Control.Exception ( throwIO )
import Data.Kind ( Type )
import Prelude

-- hasql
import Hasql.Connection ( Connection )
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- pretty
import Text.PrettyPrint ( Doc, (<+>), ($$), text )

-- rel8
import Rel8.Query ( Query )
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema, ppTable )
import Rel8.Statement.Returning
  ( Returning
  , decodeReturning, emptyReturning, ppReturning
  )
import Rel8.Statement.Using ( ppUsing )
import Rel8.Statement.Where ( Where, ppWhere )

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
    , deleteWhere :: using -> Where exprs
      -- ^ Which rows should be selected for deletion.
    , returning :: Returning names a
      -- ^ What to return from the @DELETE@ statement.
    }
    -> Delete a


ppDelete :: Delete a -> Maybe Doc
ppDelete Delete {..} = do
  (usingDoc, i) <- ppUsing using
  pure $ text "DELETE FROM" <+> ppTable from
    $$ usingDoc
    $$ ppWhere from (deleteWhere i)
    $$ ppReturning from returning


-- | Run a 'Delete' statement.
delete :: Connection -> Delete a -> IO a
delete connection d@Delete {returning} =
  case show <$> ppDelete d of
    Nothing -> pure (emptyReturning returning)
    Just sql ->
      Hasql.run session connection >>= either throwIO pure
      where
        session = Hasql.statement () statement
        statement = Hasql.Statement bytes params decode prepare
        bytes = encodeUtf8 $ Text.pack sql
        params = Hasql.noParams
        decode = decodeReturning returning
        prepare = False
