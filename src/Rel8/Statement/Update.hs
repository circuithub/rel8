{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language StandaloneKindSignatures #-}
{-# language StrictData #-}

module Rel8.Statement.Update (
  Update (..),
  update,
  ppUpdate,
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
import Rel8.Schema.Table ( TableSchema(..), ppTable )
import Rel8.Statement (Statement)
import Rel8.Statement.Returning (Returning, ppReturning, runReturning)
import Rel8.Statement.Set ( ppSet )
import Rel8.Statement.Using ( ppFrom )
import Rel8.Statement.Where ( ppWhere )

-- transformers
import Control.Monad.Trans.State.Strict (State)


-- | The constituent parts of an @UPDATE@ statement.
type Update :: Type -> Type
data Update a where
  Update ::
    Selects names exprs =>
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
    } ->
    Update a


-- | Build an @UPDATE@ 'Statement'.
update :: Update a -> Statement a
update statement@Update {returning} =
  runReturning (ppUpdate statement) returning


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
