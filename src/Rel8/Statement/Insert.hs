{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

module Rel8.Statement.Insert
  ( Insert(..)
  , insert
  , OnConflict(..)
  , Upsert(..)
  , ppInsert
  , ppInto
  )
where

-- base
import Control.Exception ( throwIO )
import Data.Foldable ( toList )
import Data.Kind ( Type )
import Prelude

-- hasql
import Hasql.Connection ( Connection )
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye

-- pretty
import Text.PrettyPrint ( Doc, (<+>), ($$), parens, text )

-- rel8
import Rel8.Category.Projection ( Projection, project )
import Rel8.Query ( Query )
import Rel8.Schema.Name ( Name, Selects, ppColumn )
import Rel8.Schema.Table ( TableSchema(..), ppTable )
import Rel8.Statement.Returning ( Returning(..), ppReturning )
import Rel8.Statement.Select ( ppSelect )
import Rel8.Statement.Update ( Set, ppSet )
import Rel8.Statement.Where ( Where, ppWhere )
import Rel8.Table ( Table )
import Rel8.Table.Name ( showNames )
import Rel8.Table.Opaleye ( attributes )
import Rel8.Table.Serialize ( Serializable, parse )

-- text
import qualified Data.Text as Text ( pack )
import Data.Text.Encoding ( encodeUtf8 )


-- | @OnConflict@ allows you to add an @ON CONFLICT@ clause to an @INSERT@
-- statement.
type OnConflict :: Type -> Type
data OnConflict names
  = Abort
  -- ^ @ON CONFLICT DO ABORT@
  | DoNothing
  -- ^ @ON CONFLICT DO NOTHING@
  | DoUpdate (Upsert names)
  -- ^ @ON CONFLICT DO UPDATE@


-- | The @ON CONFLICT (...) DO UPDATE@ clause of an @INSERT@ statement, also
-- known as \"upsert\".
--
-- For upsert, Postgres requires an explicit set of \"conflict targets\" —
-- the set of columns composing the @UNIQUE@ index from conflicts with which
-- we would like to recover.
--
-- The other thing to keep in mind is that the @SET@ and @WHERE@ clauses of
-- an upsert can reference not only the existing row in the database, but also
-- the row that we tried to insert which conflicted with the existing row
-- (what Postgres calls the @excluded@ row).
type Upsert :: Type -> Type
data Upsert names where
  Upsert :: (Selects names exprs, Table Name conflicts) =>
    { conflicts :: Projection names conflicts
      -- ^ The set of conflict targets, projected from the set of columns for
      -- the whole table
    , set :: exprs -> Set exprs
      -- ^ How to update each selected row. The first @exprs@ argument is the
      -- @excluded@ row.
    , updateWhere :: exprs -> Where exprs
      -- ^ Which rows to select for update. The first @exprs@ argument is the
      -- @excluded@ row.
    }
    -> Upsert names


ppOnConflict :: TableSchema names -> OnConflict names -> Doc
ppOnConflict schema = \case
  Abort -> mempty
  DoNothing -> text "ON CONFLICT DO NOTHING"
  DoUpdate upsert -> case ppUpsert schema upsert of
    Nothing -> text "ON CONFLICT DO NOTHING"
    Just doc -> doc


ppUpsert :: TableSchema names -> Upsert names -> Maybe Doc
ppUpsert schema@TableSchema {columns} Upsert {..} = do
  condition <- ppWhere schema (updateWhere excluded)
  pure $
    text "ON CONFLICT" <+>
    ppConflicts schema conflicts <+>
    text "DO UPDATE" $$
    ppSet schema (set excluded) $$
    condition
  where
    excluded = attributes TableSchema
      { schema = Nothing
      , name = "excluded"
      , columns
      }


ppConflicts :: (Table Name names, Table Name conflicts)
  => TableSchema names -> Projection names conflicts -> Doc
ppConflicts TableSchema {columns} conflicts =
  Opaleye.commaV ppColumn $ toList $ showNames $ project conflicts columns


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


ppInsert :: Insert a -> Maybe Doc
ppInsert Insert {..} = do
  rows' <- ppSelect rows
  pure $ text "INSERT INTO" <+> ppInto into
    $$ rows'
    $$ ppOnConflict into onConflict
    $$ ppReturning into returning


ppInto :: Table Name a => TableSchema a -> Doc
ppInto table@TableSchema {columns} =
  ppTable table <+>
  parens (Opaleye.commaV ppColumn (toList (showNames columns)))


-- | Run an @INSERT@ statement
insert :: Connection -> Insert a -> IO a
insert c i@Insert {returning} =
  case (show <$> ppInsert i, returning) of
    (Nothing, NumberOfRowsAffected) -> pure 0
    (Nothing, Projection _) -> pure []

    (Just sql, NumberOfRowsAffected) -> Hasql.run session c >>= either throwIO pure
      where
        session = Hasql.statement () statement
        statement = Hasql.Statement bytes params decode prepare
        bytes = encodeUtf8 $ Text.pack sql
        params = Hasql.noParams
        decode = Hasql.rowsAffected
        prepare = False

    (Just sql, Projection f) -> Hasql.run session c >>= either throwIO pure
      where
        session = Hasql.statement () statement
        statement = Hasql.Statement bytes params decode prepare
        bytes = encodeUtf8 $ Text.pack sql
        params = Hasql.noParams
        decode = decoder f
        prepare = False

  where
    decoder :: forall exprs projection a. Serializable projection a
      => (exprs -> projection) -> Hasql.Result [a]
    decoder _ = Hasql.rowList (parse @projection @a)
