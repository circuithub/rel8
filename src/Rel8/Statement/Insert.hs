{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}

module Rel8.Statement.Insert
  ( Insert(..)
  , insert
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
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye

-- pretty
import Text.PrettyPrint ( Doc, (<+>), ($$), parens, text )

-- rel8
import Rel8.Query ( Query )
import Rel8.Schema.Name ( Name, Selects, ppColumn )
import Rel8.Schema.Table ( TableSchema(..), ppTable )
import Rel8.Statement.OnConflict ( OnConflict, ppOnConflict )
import Rel8.Statement.Returning
  ( Returning
  , decodeReturning, emptyReturning, ppReturning
  )
import Rel8.Statement.Select ( ppSelect )
import Rel8.Table ( Table )
import Rel8.Table.Name ( showNames )

-- text
import qualified Data.Text as Text ( pack )
import Data.Text.Encoding ( encodeUtf8 )


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


-- | Run an 'Insert' statement.
insert :: Connection -> Insert a -> IO a
insert connection i@Insert {returning} =
  case show <$> ppInsert i of
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
