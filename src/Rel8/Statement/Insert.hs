{-# LANGUAGE LambdaCase #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

module Rel8.Statement.Insert
  ( Insert(..)
  , Inserts
  , OnConflict(..)
  , insert
  )
where

-- base
import Control.Exception ( throwIO )
import Control.Monad ( (>=>) )
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Prelude

-- hasql
import Hasql.Connection ( Connection )
import qualified Hasql.Decoders as Hasql
import qualified Hasql.Encoders as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Statement as Hasql

-- rel8
import qualified Opaleye.Internal.Manipulation as Opaleye
import qualified Opaleye.Manipulation as Opaleye
import Rel8.Schema.Table ( TableSchema )
import Rel8.Statement.Returning ( Returning( Projection, NumberOfRowsAffected ) )
import Rel8.Table ( fromColumns, toColumns )
import Rel8.Table.Opaleye ( table, unpackspec )
import Rel8.Table.Recontextualize ( Inserts, Selects )
import Rel8.Table.Serialize ( Serializable, parse )

-- text
import qualified Data.Text as Text ( pack )
import Data.Text.Encoding ( encodeUtf8 )


-- | The constituent parts of a SQL @INSERT@ statement.
type Insert :: Type -> Type
data Insert a where
  Insert :: (Selects names exprs, Inserts exprs inserts) =>
    { into :: TableSchema names
      -- ^ Which table to insert into.
    , rows :: [inserts]
      -- ^ The rows to insert.
    , onConflict :: OnConflict
      -- ^ What to do if the inserted rows conflict with data already in the
      -- table.
    , returning :: Returning names a
      -- ^ What information to return on completion.
    }
    -> Insert a

type Inserts :: Type -> Type -> Constraint
class Recontextualize Expr Insert exprs inserts => Inserts exprs inserts
instance Recontextualize Expr Insertion exprs inserts => Inserts exprs inserts
instance {-# OVERLAPPING #-} Inserts (Opaque1 Expr Opaque) (Opaque1 Insertion Opaque)


instance Labelable Insert where
  labeler = \case
    RequiredInsert a -> RequiredInsert a
    OptionalInsert ma -> OptionalInsert ma

  unlabeler = \case
    RequiredInsert a -> RequiredInsert a
    OptionalInsert ma -> OptionalInsert ma


-- | @OnConflict@ allows you to add an @ON CONFLICT@ clause to an @INSERT@
-- statement.
data OnConflict 
  = Abort     -- ^ @ON CONFLICT ABORT@
  | DoNothing -- ^ @ON CONFLICT DO NOTHING@


-- | Run an @INSERT@ statement
--
-- >>> :{
-- insert c Insert
--   { into = authorSchema
--   , rows = [ lit Author{ authorName = "Gabriel Gonzales", authorId = AuthorId 4, authorUrl = Just "https://haskellforall.com" } ]
--   , onConflict = Abort
--   , returning = NumberOfRowsAffected
--   }
-- :}
-- 1
insert :: Insert a -> Connection -> IO a
insert Insert {into, rows, onConflict, returning} =
  case (rows, returning) of
    ([], NumberOfRowsAffected) -> const $ pure 0
    ([], Projection _) -> const $ pure []

    (x:xs, NumberOfRowsAffected) -> Hasql.run session >=> either throwIO pure
      where
        session = Hasql.statement () statement
        statement = Hasql.Statement bytes params decode prepare
        bytes = encodeUtf8 $ Text.pack sql
        params = Hasql.noParams
        decode = Hasql.rowsAffected
        prepare = False
        sql = Opaleye.arrangeInsertManySql into' rows' onConflict'
          where
            into' = table $ toColumns <$> into
            rows' = toColumns <$> x :| xs

    (x:xs, Projection project) -> Hasql.run session >=> either throwIO pure
      where
        session = Hasql.statement () statement
        statement = Hasql.Statement bytes params decode prepare
        bytes = encodeUtf8 $ Text.pack sql
        params = Hasql.noParams
        decode = decoder project
        prepare = False
        sql =
          Opaleye.arrangeInsertManyReturningSql
            unpackspec
            into'
            rows'
            project'
            onConflict'
          where
            into' = table $ toColumns <$> into
            rows' = toColumns <$> x :| xs
            project' = toColumns . project . fromColumns

  where
    onConflict' =
      case onConflict of
        DoNothing -> Just Opaleye.DoNothing
        Abort     -> Nothing

    decoder :: forall exprs projection a. Serializable projection a
      => (exprs -> projection) -> Hasql.Result [a]
    decoder _ = Hasql.rowList (parse @projection @a)


table ::(Selects names exprs, Inserts exprs inserts)
  => TableSchema names -> Opaleye.Table inserts exprs
table (TableSchema name schema columns) =
  Opaleye.Table qualified (tableFields columns)
  where
    qualified = case schema of
      Nothing -> name
      Just qualifier -> qualifier <> "." <> name


tableFields :: (Selects names exprs, Inserts exprs inserts)
  => names -> Opaleye.TableFields inserts exprs
tableFields (toColumns -> names) = dimap toColumns fromColumns $
  unwrapApplicative $ htabulateA $ \field -> WrapApplicative $
    case hfield hspecs field of
      specs -> case hfield names field of
        name -> lmap (`hfield` field) (go specs name)
  where
    go :: SSpec spec -> Name spec -> Opaleye.TableFields (Insertion spec) (DB spec)
    go SSpec {necessity, info, nullability} (Name name) =
      case necessity of
        SRequired ->
          lmap (\(RequiredInsert a) -> toColumn $ toPrimExpr a) $
          DB . scastExpr nullability info . fromPrimExpr . fromColumn <$>
            Opaleye.requiredTableField name
        SOptional ->
          lmap (\(OptionalInsert ma) -> toColumn . toPrimExpr <$> ma) $
          DB . scastExpr nullability info . fromPrimExpr . fromColumn <$>
            Opaleye.optionalTableField name


toInsert :: Inserts exprs inserts => exprs -> inserts
toInsert (toColumns -> exprs) = fromColumns $ htabulate $ \field ->
  case hfield hspecs field of
    SSpec {necessity} -> case hfield exprs field of
      DB expr -> case necessity of
        SRequired -> RequiredInsert expr
        SOptional -> OptionalInsert (Just expr)


toInsertDefaults :: Inserts exprs inserts => exprs -> inserts
toInsertDefaults (toColumns -> exprs) = fromColumns $ htabulate $ \field ->
  case hfield hspecs field of
    SSpec {necessity} -> case hfield exprs field of
      DB expr -> case necessity of
        SRequired -> RequiredInsert expr
        SOptional -> OptionalInsert Nothing
