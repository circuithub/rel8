{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}
{-# language TypeApplications #-}

module Rel8.Statement.Insert
  ( Insert(..)
  , insert
  , insert_
  , ppInsert
  , ppInto
  )
where

-- base
import Data.Foldable ( toList )
import Data.Kind ( Type )
import Data.Int (Int64)
import Prelude

-- hasql
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Statement as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye

-- pretty
import Text.PrettyPrint ( Doc, (<+>), ($$), parens, text )

-- rel8
import Rel8.Query ( Query )
import Rel8.Schema.Name ( Name, Selects, ppColumn )
import Rel8.Schema.Table ( TableSchema(..), ppTable )
import Rel8.Statement.OnConflict ( OnConflict, ppOnConflict )
import Rel8.Statement.Returning
  ( Returning(Returning)
  , ppReturning
  )
import Rel8.Statement.Select ( ppRows )
import Rel8.Table ( Table )
import Rel8.Table.Name ( showNames )
import Rel8.Table.Serialize (Serializable, parse)

-- text
import qualified Data.Text as Text ( pack )
import Data.Text.Encoding ( encodeUtf8 )

-- transformers
import Control.Monad.Trans.State.Strict (State, evalState)


-- | The constituent parts of a SQL @INSERT@ statement.
type Insert :: Type -> Type
data Insert a where
  Insert :: Selects names exprs =>
    { into :: TableSchema names
      -- ^ Which table to insert into.
    , rows :: Query exprs
      -- ^ The rows to insert. This can be an arbitrary query — use
      -- 'Rel8.values' insert a static list of rows.
    , onConflict :: OnConflict names
      -- ^ What to do if the inserted rows conflict with data already in the
      -- table.
    , returning :: Returning names a
      -- ^ What information to return on completion.
    }
    -> Insert a


ppInsert :: Insert a -> State Opaleye.Tag Doc
ppInsert Insert {..} = do
  rows' <- ppRows rows
  pure $
    text "INSERT INTO" <+>
    ppInto into $$
    rows' $$
    ppOnConflict into onConflict $$
    ppReturning into returning


ppInto :: Table Name a => TableSchema a -> Doc
ppInto table@TableSchema {columns} =
  ppTable table <+>
  parens (Opaleye.commaV ppColumn (toList (showNames columns)))


-- | Run an 'Insert' statement and select all rows of the 'Returning' clause.
insert :: Serializable exprs a => Insert (Query exprs) -> Hasql.Statement () [a]
insert i@Insert {returning} = Hasql.Statement bytes params decode prepare
  where
    bytes = encodeUtf8 $ Text.pack sql
    params = Hasql.noParams
    decode = case returning of
      Returning (_ :: exprs -> returning) -> Hasql.rowList (parse @returning)
    prepare = False
    sql = show doc
    doc = evalState (ppInsert i) Opaleye.start


-- | Run an 'Insert' statement and return the number of rows affected.
insert_ :: Insert a -> Hasql.Statement () Int64
insert_ i = Hasql.Statement bytes params decode prepare
  where
    bytes = encodeUtf8 $ Text.pack sql
    params = Hasql.noParams
    decode = Hasql.rowsAffected
    prepare = False
    sql = show doc
    doc = evalState (ppInsert i) Opaleye.start
