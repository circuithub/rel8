{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

module Rel8.Statement.Update
  ( Update(..)
  , update
  , Set
  , ppUpdate
  , ppSet
  )
where

-- base
import Control.Exception ( throwIO )
import Data.Foldable ( toList )
import Data.Kind ( Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.Sql.Print as Opaleye
import qualified Opaleye.Internal.Sql as Opaleye

-- pretty
import Text.PrettyPrint ( Doc, (<+>), ($$), equals, text )

-- hasql
import Hasql.Connection ( Connection )
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- rel8
import Rel8.Schema.Name ( Selects, ppColumn )
import Rel8.Schema.Table ( TableSchema(..), ppTable )
import Rel8.Statement.Returning ( Returning(..), ppReturning )
import Rel8.Statement.Where ( Where, ppWhere )
import Rel8.Table.Opaleye ( exprsWithNames, view )
import Rel8.Table.Serialize ( Serializable, parse )

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )


-- | The constituent parts of an @UPDATE@ statement.
type Update :: Type -> Type
data Update a where
  Update :: Selects names exprs =>
    { target :: TableSchema names
      -- ^ Which table to update.
    , set :: Set exprs
      -- ^ How to update each selected row.
    , updateWhere :: Where exprs
      -- ^ Which rows to select for update.
    , returning :: Returning names a
      -- ^ What to return from the @UPDATE@ statement.
    }
    -> Update a


-- | The @SET@ part of an @UPDATE@ (or @ON CONFLICT DO UPDATE@) statement.
type Set expr = expr -> expr


ppUpdate :: Update a -> Maybe Doc
ppUpdate Update {..} = do
  condition <- ppWhere target updateWhere
  pure $
    text "UPDATE" <+>
    ppTable target $$
    ppSet target set $$
    condition $$ ppReturning target returning


ppSet :: Selects names exprs => TableSchema names -> Set exprs -> Doc
ppSet TableSchema {columns} f =
  text "SET" <+> Opaleye.commaV ppAssign (toList assigns)
  where
    assigns =
      fmap Opaleye.sqlExpr <$> exprsWithNames columns (f (view columns))
    ppAssign (column, expr) =
      ppColumn column <+> equals <+> Opaleye.ppSqlExpr expr


-- | Run an @UPDATE@ statement.
update :: Connection -> Update a -> IO a
update c u@Update {returning} =
  case (show <$> ppUpdate u, returning) of
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
