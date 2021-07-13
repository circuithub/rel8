{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}

module Rel8.Statement.Update
  ( Update(..)
  , update
  , ppUpdate
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
import Rel8.Schema.Table ( TableSchema(..), ppTable )
import Rel8.Statement.Returning ( Returning, decodeReturning, ppReturning )
import Rel8.Statement.Set ( ppSet )
import Rel8.Statement.Using ( ppFrom )
import Rel8.Statement.Where ( ppWhere )

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )


-- | The constituent parts of an @UPDATE@ statement.
type Update :: Type -> Type
data Update a where
  Update :: Selects names exprs =>
    { target :: TableSchema names
      -- ^ Which table to update.
    , from :: Query from
      -- ^ @FROM@ clause — this can be used to join against other tables,
      -- and its results can be referenced in the @SET@ and @WHERE@ clauses.
    , set :: from -> exprs -> exprs
      -- ^ How to update each selected row.
    , updateWhere :: from -> exprs -> Expr Bool
      -- ^ Which rows to select for update.
    , returning :: Returning names a
      -- ^ What to return from the @UPDATE@ statement.
    }
    -> Update a


ppUpdate :: Update a -> Doc
ppUpdate Update {..} = case ppFrom from of
  Nothing ->
    text "UPDATE" <+> ppTable target $$
    ppSet target id $$
    text "WHERE false"
  Just (fromDoc, i) ->
    text "UPDATE" <+> ppTable target $$
    ppSet target (set i) $$
    fromDoc $$
    ppWhere target (updateWhere i) $$
    ppReturning target returning


-- | Run an @UPDATE@ statement.
update :: Update a -> Hasql.Statement () a
update u@Update {returning} = Hasql.Statement bytes params decode prepare
  where
    bytes = encodeUtf8 $ Text.pack sql
    params = Hasql.noParams
    decode = decodeReturning returning
    prepare = False
    sql = show doc
    doc = ppUpdate u
