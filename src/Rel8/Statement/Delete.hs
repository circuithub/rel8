{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}

module Rel8.Statement.Delete (
  Delete (..),
  delete,
  ppDelete,
)
where

-- base
import Data.Kind (Type)
import Prelude

-- opaleye
import qualified Opaleye.Internal.Tag as Opaleye

-- pretty
import Text.PrettyPrint (Doc, text, ($$), (<+>))

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Query ( Query )
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema, ppTable )
import Rel8.Statement (Statement)
import Rel8.Statement.Returning (Returning, ppReturning, runReturning)
import Rel8.Statement.Using ( ppUsing )
import Rel8.Statement.Where ( ppWhere )

-- transformers
import Control.Monad.Trans.State.Strict (State)


-- | The constituent parts of a @DELETE@ statement.
type Delete :: Type -> Type
data Delete a where
  Delete ::
    Selects names exprs =>
    { from :: TableSchema names
    -- ^ Which table to delete from.
    , using :: Query using
    -- ^ @USING@ clause — this can be used to join against other tables,
    -- and its results can be referenced in the @WHERE@ clause
    , deleteWhere :: using -> exprs -> Expr Bool
    -- ^ Which rows should be selected for deletion.
    , returning :: Returning names a
    -- ^ What to return from the @DELETE@ statement.
    } ->
    Delete a


-- | Build a @DELETE@ 'Statement'.
delete :: Delete a -> Statement a
delete statement@Delete {returning} =
  runReturning (ppDelete statement) returning


ppDelete :: Delete a -> State Opaleye.Tag Doc
ppDelete Delete {..} = do
  musing <- ppUsing using
  pure $ case musing of
    Nothing ->
      text "DELETE FROM" <+> ppTable from $$
      text "WHERE false"
    Just (usingDoc, i) ->
      text "DELETE FROM" <+> ppTable from $$
      usingDoc $$
      ppWhere from (deleteWhere i) $$
      ppReturning from returning
