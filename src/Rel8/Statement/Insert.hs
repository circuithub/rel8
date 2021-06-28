{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Statement.Insert
  ( Insert
  , insert
  , rows
  , onConflict
  , OnConflict
  , doNothing
  , doUpdate
  , ppInsert
  , ppInto
  )
where

-- base
import Control.Exception ( throwIO )
import Data.Foldable ( toList )
import Data.Function ( on )
import Data.Functor.Compose ( Compose( Compose ) )
import Data.Kind ( Type )
import Data.List.NonEmpty ( nonEmpty )
import Prelude

-- free
import Control.Applicative.Free ( Ap, liftAp )

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
import Rel8.Category.Projection ( Projection( Projection ) )
import Rel8.Query ( Query )
import Rel8.Query.Set ( unionAll )
import Rel8.Schema.Name ( Name, Selects, ppColumn )
import Rel8.Schema.Table ( TableSchema(..), ppTable )
import Rel8.Statement.Returning
  ( Returning
  , ReturningF(..), decodeReturning, emptyReturning, ppReturning
  )
import qualified Rel8.Statement.Returning
import Rel8.Statement.Select ( ppSelect )
import Rel8.Statement.Update ( Update( Update ), UpdateF(..), ppSet )
import Rel8.Statement.Where ( ppWhere )
import Rel8.Table ( Table, toColumns )
import Rel8.Table.Name ( showNames )
import Rel8.Table.Opaleye ( attributes )

-- text
import qualified Data.Text as Text ( pack )
import Data.Text.Encoding ( encodeUtf8 )


-- | The @ON CONFLICT@ clause of an @INSERT@ statement.
type OnConflict :: Type -> Type
data OnConflict exprs where
  DoNothing :: OnConflict exprs
  DoUpdate :: (Selects names exprs, Table Name conflicts)
    => Projection names conflicts
    -> (exprs -> UpdateF exprs)
    -> OnConflict exprs


instance Semigroup (OnConflict exprs) where
  (<>) = const


-- | @ON CONFLICT DO NOTHING@
doNothing :: OnConflict exprs
doNothing = DoNothing


-- | @ON CONFLICT (...) DO UPDATE@, also known as \"upsert\".
--
-- For upsert, Postgres requires an explicit set of \"conflict targets\" —
-- the set of columns composing the @UNIQUE@ index from conflicts with which
-- we would like to recover.
--
-- The other thing to keep in mind is that the @SET@ and @WHERE@ clauses of
-- an upsert can reference not only the existing row in the database, but also
-- the row that we tried to insert which conflicted with the existing row
-- (what Postgres calls the @excluded@ row).
doUpdate :: (Selects names exprs, Table Name conflicts)
  => Projection names conflicts
  -- ^ The set of conflict targets, projected from the set of columns for the
  -- whole table
  -> (exprs -> Update exprs a)
  -- ^ Given the @excluded@ row we tried to insert, return the @UPDATE@
  -- statement to run instead
  -> OnConflict exprs
doUpdate conflicts f =
  DoUpdate conflicts ((\(Update (update, _)) -> update) <$> f)


type InsertF :: Type -> Type
data InsertF exprs = InsertF
  { rows_ :: [Query exprs]
  , onConflict_ :: Maybe (OnConflict exprs)
  }


instance Semigroup (InsertF exprs) where
  a <> b = InsertF
    { rows_ = ((<>) `on` rows_) b a
    , onConflict_ = ((<>) `on` onConflict_) a b
    }


instance Monoid (InsertF exprs) where
  mempty = InsertF
    { rows_ = []
    , onConflict_ = Nothing
    }


-- | An 'Applicative' that builds an @INSERT@ stataement.
type Insert :: Type -> Type -> Type
newtype Insert exprs a = Insert (InsertF exprs, Ap (ReturningF exprs) a)
  deriving (Functor, Applicative) via
    Compose ((,) (InsertF exprs)) (Ap (ReturningF exprs))


instance Returning Insert where
  numberOfRowsAffected = Insert (pure (liftAp NumberOfRowsAffected))
  returning projection = Insert (pure (liftAp (Returning projection)))


-- | Add the given rows to the 'Insert' statement. This can be an arbitrary
-- query — use 'Rel8.values' to insert a static list of rows.
rows :: Query exprs -> Insert exprs ()
rows query = Insert (InsertF [query] Nothing, pure ())


-- | 'onConflict' allows you to add an @ON CONFLICT@ clause to an 'Insert'
-- statement.
onConflict :: OnConflict exprs -> Insert exprs ()
onConflict oc = Insert (InsertF [] (Just oc), pure ())


ppOnConflict :: Selects names exprs
  => TableSchema names -> OnConflict exprs -> Doc
ppOnConflict schema@TableSchema {columns} = \case
  DoNothing -> text "ON CONFLICT DO NOTHING"
  DoUpdate conflicts f -> case f excluded of
    UpdateF {..} -> case ppWhere schema where_ of
      Nothing -> text "ON CONFLICT DO NOTHING"
      Just condition ->
        text "ON CONFLICT" <+>
        ppConflicts schema conflicts <+>
        text "DO UPDATE" $$
        ppSet schema set_ $$
        condition
  where
    excluded = attributes TableSchema
      { schema = Nothing
      , name = "excluded"
      , columns
      }


ppConflicts ::
  ( Selects names exprs, Selects names' exprs
  , Table Name conflicts
  )
  => TableSchema names -> Projection names' conflicts -> Doc
ppConflicts TableSchema {columns} (Projection conflicts) =
  Opaleye.commaV ppColumn $ toList $ showNames $ conflicts (toColumns columns)


ppInsert :: Selects names exprs
  => TableSchema names -> Insert exprs a -> Maybe Doc
ppInsert into (Insert (InsertF {..}, returning)) = do
  rows' <- nonEmpty rows_ >>= ppSelect . foldl1 unionAll
  pure $ text "INSERT INTO" <+> ppInto into
    $$ rows'
    $$ foldMap (ppOnConflict into) onConflict_
    $$ ppReturning into returning


ppInto :: Table Name a => TableSchema a -> Doc
ppInto table@TableSchema {columns} =
  ppTable table <+>
  parens (Opaleye.commaV ppColumn (toList (showNames columns)))


-- | Run an 'Insert' statement.
insert :: Selects names exprs
  => Connection -> TableSchema names -> Insert exprs a -> IO a
insert connection into i@(Insert (_, returning)) =
  case show <$> ppInsert into i of
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
