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
import Data.Foldable ( toList )
import Data.Kind ( Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye

-- pretty
import Text.PrettyPrint ( Doc, (<+>), ($$), parens, text )

-- rel8
import Rel8.Query ( Query )
import Rel8.Schema.Name ( Name, Selects, ppColumn )
import Rel8.Schema.Table ( TableSchema(..), ppTable )
import Rel8.Statement (Statement)
import Rel8.Statement.OnConflict ( OnConflict, ppOnConflict )
import Rel8.Statement.Returning (Returning, ppReturning, runReturning)
import Rel8.Statement.Select ( ppRows )
import Rel8.Table ( Table )
import Rel8.Table.Name ( showNames )

-- transformers
import Control.Monad.Trans.State.Strict (State)


-- | The constituent parts of a SQL @INSERT@ statement.
type Insert :: Type -> Type
data Insert a where
  Insert :: Selects names exprs =>
    { into :: TableSchema names
      -- ^ Which table to insert into.
    , rows :: Query exprs
      -- ^ The rows to insert. This can be an arbitrary query — use
      -- 'Rel8.values' insert a static list of rows.
    , onConflict :: OnConflict exprs
      -- ^ What to do if the inserted rows conflict with data already in the
      -- table.
    , returning :: Returning names a
      -- ^ What information to return on completion.
    }
    -> Insert a


-- | Build an @INSERT@ 'Statement'.
insert :: Insert a -> Statement a
insert statement@Insert {returning} =
  runReturning (ppInsert statement) returning


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
