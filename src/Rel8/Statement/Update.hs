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
import Rel8.Expr ( Expr )
import Rel8.Query ( Query )
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema(..), ppTable )
import Rel8.Statement.Returning
  ( Returning
  , decodeReturning, emptyReturning, ppReturning
  )
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


ppUpdate :: Update a -> Maybe Doc
ppUpdate Update {..} = do
  (fromDoc, i) <- ppFrom from
  pure $
    text "UPDATE" <+>
    ppTable target $$
    ppSet target (set i) $$
    fromDoc $$
    ppWhere target (updateWhere i) $$
    ppReturning target returning


-- | Run an @UPDATE@ statement.
update :: Connection -> Update a -> IO a
update connection u@Update {returning} =
  case show <$> ppUpdate u of
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
