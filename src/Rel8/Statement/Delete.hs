{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

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
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- pretty
import Text.PrettyPrint ( Doc, (<+>), ($$), text )

-- rel8
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema, ppTable )
import Rel8.Statement.Returning ( Returning(..), ppReturning )
import Rel8.Statement.Where ( Where, ppWhere )
import Rel8.Table.Serialize ( Serializable, parse )

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )


-- | The constituent parts of a @DELETE@ statement.
type Delete :: Type -> Type
data Delete a where
  Delete :: Selects names exprs =>
    { from :: TableSchema names
      -- ^ Which table to delete from.
    , deleteWhere :: Where exprs
      -- ^ Which rows should be selected for deletion.
    , returning :: Returning names a
      -- ^ What to return from the @DELETE@ statement.
    }
    -> Delete a


ppDelete :: Delete a -> Maybe Doc
ppDelete Delete {..} = do
  condition <- ppWhere from deleteWhere
  pure $ text "DELETE FROM" <+> ppTable from
    $$ condition
    $$ ppReturning from returning


-- | Run a @DELETE@ statement.
delete :: Connection -> Delete a -> IO a
delete c d@Delete {returning} =
  case (show <$> ppDelete d, returning) of
    (Nothing, NumberOfRowsAffected) -> pure 0
    (Nothing, Projection _) -> pure []
    (Just sql, NumberOfRowsAffected) ->
      Hasql.run session c >>= either throwIO pure
      where
        session = Hasql.statement () statement
        statement = Hasql.Statement bytes params decode prepare
        bytes = encodeUtf8 $ Text.pack sql
        params = Hasql.noParams
        decode = Hasql.rowsAffected
        prepare = False

    (Just sql, Projection project) ->
      Hasql.run session c >>= either throwIO pure
      where
        session = Hasql.statement () statement
        statement = Hasql.Statement bytes params decode prepare
        bytes = encodeUtf8 $ Text.pack sql
        params = Hasql.noParams
        decode = decoder project
        prepare = False
  where
    decoder :: forall exprs projection a. Serializable projection a
      => (exprs -> projection) -> Hasql.Result [a]
    decoder _ = Hasql.rowList (parse @projection @a)
