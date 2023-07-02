{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}

module Rel8.Statement.View
  ( createView
  , createOrReplaceView
  )
where

-- base
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
import Rel8.Query ( Query )
import Rel8.Schema.Name ( Selects )
import Rel8.Schema.Table ( TableSchema )
import Rel8.Statement.Insert ( ppInto )
import Rel8.Statement.Select ( ppSelect )

-- text
import qualified Data.Text as Text
import Data.Text.Encoding ( encodeUtf8 )

-- transformers
import Control.Monad.Trans.State.Strict (evalState)


data CreateView = Create | CreateOrReplace


-- | Given a 'TableSchema' and 'Query', @createView@ runs a @CREATE VIEW@
-- statement that will save the given query as a view. This can be useful if
-- you want to share Rel8 queries with other applications.
createView :: Selects names exprs
  => TableSchema names -> Query exprs -> Hasql.Statement () ()
createView =
  createViewGeneric Create


-- | Given a 'TableSchema' and 'Query', @createOrReplaceView@ runs a
-- @CREATE OR REPLACE VIEW@ statement that will save the given query
-- as a view, replacing the current view definition if it exists and
-- adheres to the restrictions in place for replacing a view in
-- PostgreSQL.
createOrReplaceView :: Selects names exprs
  => TableSchema names -> Query exprs -> Hasql.Statement () ()
createOrReplaceView =
  createViewGeneric CreateOrReplace


createViewGeneric :: Selects names exprs
  => CreateView -> TableSchema names -> Query exprs -> Hasql.Statement () ()
createViewGeneric replace schema query =
  Hasql.Statement bytes params decode prepare
  where
    bytes = encodeUtf8 (Text.pack sql)
    params = Hasql.noParams
    decode = Hasql.noResult
    prepare = False
    sql = show doc
    doc = ppCreateView schema query replace


ppCreateView :: Selects names exprs
  => TableSchema names -> Query exprs -> CreateView -> Doc
ppCreateView schema query replace =
  createOrReplace replace <+>
  ppInto schema $$
  text "AS" <+>
  evalState (ppSelect query) Opaleye.start
  where
    createOrReplace Create = text "CREATE VIEW"
    createOrReplace CreateOrReplace = text "CREATE OR REPLACE VIEW"
