{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}
{-# language TypeApplications #-}

module Rel8.Statement.Update
  ( Update(..)
  , update
  , update_
  , ppUpdate
  )
where

-- base
import Data.Kind ( Type )
import Data.Int ( Int64 )
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
import Rel8.Schema.Table ( TableSchema(..), ppTable )
import Rel8.Statement.Returning
  ( Returning(Returning)
  , ppReturning
  )
import Rel8.Statement.Set ( ppSet )
import Rel8.Statement.Using ( ppFrom )
import Rel8.Statement.Where ( ppWhere )
import Rel8.Table.Serialize (Serializable, parse)

-- text
import qualified Data.Text as Text ( pack )
import Data.Text.Encoding ( encodeUtf8 )

-- transformers
import Control.Monad.Trans.State.Strict (State, evalState)


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


ppUpdate :: Update a -> State Opaleye.Tag Doc
ppUpdate Update {..} = do
  mfrom <- ppFrom from
  pure $ case mfrom of
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


-- | Run an @UPDATE .. RETURNING@ statement.
update :: Serializable exprs a => Update (Query exprs) -> Hasql.Statement () [a]
update u@Update {returning} = Hasql.Statement bytes params decode prepare
  where
    bytes = encodeUtf8 $ Text.pack sql
    params = Hasql.noParams
    decode = case returning of
      Returning (_ :: exprs -> returning) -> Hasql.rowList (parse @returning)
    prepare = False
    sql = show doc
    doc = evalState (ppUpdate u) Opaleye.start


-- | Run an @UPDATE@ statement and return the number of rows affected.
update_ :: Update a -> Hasql.Statement () Int64
update_ u = Hasql.Statement bytes params decode prepare
  where
    bytes = encodeUtf8 $ Text.pack sql
    params = Hasql.noParams
    decode = Hasql.rowsAffected
    prepare = False
    sql = show doc
    doc = evalState (ppUpdate u) Opaleye.start
