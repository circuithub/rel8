{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedRecordDot #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# options_ghc -Wno-partial-fields #-}

module Rel8.Table.Verify
  ( getSchemaErrors
  , SomeTableSchema(..)
  , showCreateTable
  , checkedShowCreateTable
  )
where

-- base
import Data.Bits (shiftR, (.&.))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Const
import Data.Functor.Contravariant ( (>$<) )
import Data.Int ( Int16, Int64 )
import qualified Data.List as L
import Data.List.NonEmpty ( NonEmpty((:|)) )
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (isJust, mapMaybe)
import Data.Text ( Text )
import qualified Data.Text as T
import GHC.Generics
import Prelude hiding ( filter )
import qualified Prelude as P

-- containers
import qualified Data.Map as M

-- hasql
import qualified Hasql.Statement as HS

-- rel8
import Rel8.Column ( Column )
import Rel8.Column.List ( HList )
import Rel8.Expr ( Expr )
import Rel8.Expr.Eq ((==.))
import Rel8.Expr.Ord ((>.))
import Rel8.Expr.Order (asc)
import Rel8.Generic.Rel8able (GFromExprs, Rel8able)
import Rel8.Query ( Query )
import Rel8.Query.Each (each)
import Rel8.Query.Filter (filter)
import Rel8.Query.List (many)
import Rel8.Query.Order (orderBy)
import Rel8.Schema.HTable
import Rel8.Schema.Name ( Name(Name) )
import Rel8.Schema.Null hiding (nullable)
import Rel8.Schema.QualifiedName ( QualifiedName(..) )
import Rel8.Schema.Result ( Result )
import Rel8.Schema.Spec (Spec (Spec))
import qualified Rel8.Schema.Spec
import Rel8.Schema.Table ( TableSchema(..) )
import Rel8.Statement.Run (run1)
import Rel8.Statement.Select (select)
import Rel8.Table (Columns, toColumns)
import Rel8.Table.List ( ListTable )
import Rel8.Table.Name (namesFromLabelsWith)
import Rel8.Table.Rel8able ()
import Rel8.Table.Serialize (ToExprs, lit)
import Rel8.Type ( DBType(..) )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Information (parseTypeInformation)
import qualified Rel8.Type.Information
import Rel8.Type.Name ( TypeName(..) )

-- semialign
import Data.Semialign (align)

-- these
import Data.These


data Relkind
    = OrdinaryTable
    | Index
    | Sequence
    | ToastTable
    | View
    | MaterializedView
    | CompositeType
    | ForeignTable
    | PartitionedTable
    | PartitionedIndex
  deriving stock (Show)
  deriving anyclass (DBEq)

instance DBType Relkind where
  typeInformation = parseTypeInformation parser printer typeInformation
    where
      parser = \case
        "r"         -> pure OrdinaryTable
        "i"         -> pure Index
        "S"         -> pure Sequence
        "t"         -> pure ToastTable
        "v"         -> pure View
        "m"         -> pure MaterializedView
        "c"         -> pure CompositeType
        "f"         -> pure ForeignTable
        "p"         -> pure PartitionedTable
        "I"         -> pure PartitionedIndex
        (x :: Text) -> Left $ "Unknown relkind: " ++ show x

      printer = \case
        OrdinaryTable -> "r"
        Index -> "i"
        Sequence -> "S"
        ToastTable -> "t"
        View -> "v"
        MaterializedView -> "m"
        CompositeType -> "c"
        ForeignTable -> "f"
        PartitionedTable -> "p"
        PartitionedIndex -> "I"

newtype Oid = Oid Int64
  deriving newtype (DBType, DBEq, Show)

data PGClass f = PGClass
  { oid :: Column f Oid
  , relname :: Column f Text
  , relkind :: Column f Relkind
  , relnamespace :: Column f Oid
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (PGClass Result)

pgclass :: TableSchema (PGClass Name)
pgclass = TableSchema
  { name = QualifiedName "pg_class" (Just "pg_catalog")
  , columns = namesFromLabelsWith NonEmpty.last
  }

data PGAttribute f = PGAttribute
  { attrelid :: Column f Oid
  , attname :: Column f Text
  , atttypid :: Column f Oid
  , attnum :: Column f Int64
  , atttypmod :: Column f Int64
  , attnotnull :: Column f Bool
  , attndims :: Column f Int16
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (PGAttribute Result)

pgattribute :: TableSchema (PGAttribute Name)
pgattribute = TableSchema
  { name = QualifiedName "pg_attribute" (Just "pg_catalog")
  , columns = namesFromLabelsWith NonEmpty.last
  }

data PGType f = PGType
  { oid :: Column f Oid
  , typname :: Column f Text
  , typnamespace :: Column f Oid
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (PGType Result)

pgtype :: TableSchema (PGType Name)
pgtype = TableSchema
  { name = QualifiedName "pg_type" (Just "pg_catalog")
  , columns = namesFromLabelsWith NonEmpty.last
  }

data PGNamespace f = PGNamespace
  { oid :: Column f Oid
  , nspname :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (PGNamespace Result)

pgnamespace :: TableSchema (PGNamespace Name)
pgnamespace = TableSchema
  { name = QualifiedName "pg_namespace" (Just "pg_catalog")
  , columns = namesFromLabelsWith NonEmpty.last
  }

data PGCast f = PGCast
  { oid :: Column f Oid
  , castsource :: Column f Oid
  , casttarget :: Column f Oid
  , castfunc :: Column f Oid
  , castcontext :: Column f Text -- Char
  , castmethod :: Column f Char
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (PGCast Result)

pgcast :: TableSchema (PGCast Name)
pgcast = TableSchema
  { name = QualifiedName "pg_cast" (Just "pg_catalog")
  , columns = namesFromLabelsWith NonEmpty.last
  }

data PGTable f = PGTable
  { name :: Column f Text
  , columns :: HList f (Attribute f)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (PGTable Result)

data Attribute f = Attribute
  { attribute :: PGAttribute f
  , typ :: PGType f
  , namespace :: PGNamespace f
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (Attribute Result)

data Cast f = Cast
  { source :: PGType f
  , target :: PGType f
  , context :: Column f Text -- Char 
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (Cast Result)

fetchTables :: Query (ListTable Expr (PGTable Expr))
fetchTables = many do
    PGClass{ oid = tableOid, relname } <- orderBy (relname >$< asc) do
      each pgclass
        >>= filter ((lit OrdinaryTable ==.) . relkind)

    columns <- many do
      attribute@PGAttribute{ atttypid } <-
        each pgattribute
          >>= filter ((tableOid ==.) . attrelid)
          >>= filter ((>. 0) . attnum)

      typ <-
        each pgtype
          >>= filter (\PGType{ oid = typoid } -> atttypid ==. typoid)

      namespace <-
        each pgnamespace
          >>= filter (\PGNamespace{ oid = nsoid } -> nsoid ==. typ.typnamespace)



      return Attribute{ attribute, typ, namespace }

    return PGTable
      { name = relname
      , ..
      }

fetchCasts :: Query (ListTable Expr (Cast Expr))
fetchCasts = many do
    PGCast {castsource, casttarget, castcontext} <- each pgcast
    src <- each pgtype >>= filter (\PGType { oid = typoid } -> typoid ==. castsource)
    tgt <- each pgtype >>= filter (\PGType { oid = typoid } -> typoid ==. casttarget)
    return Cast { source = src, target = tgt, context = castcontext }


data CheckEnv = CheckEnv
  { schemaMap :: M.Map String [Attribute Result] -- map of schemas to attributes
  , casts :: [(String, String)] -- list of implicit casts
  } deriving (Show)


nullableToBool :: Nullity a -> Bool
nullableToBool Null = True
nullableToBool NotNull = False


attrsToMap :: [Attribute Result] -> M.Map String (Attribute Result)
attrsToMap = M.fromList . map (\attr -> (T.unpack attr.attribute.attname, attr))


data TypeInfo = TypeInfo
  { label :: [String]
  , isNull :: Bool
  , typeName :: TypeName
  }
instance Show TypeInfo where
  show = showTypeInfo


-- @'schemaToTypeMap'@ takes a schema and returns a map of database column names
-- to the type information associated with the column. It is possible (though
-- undesirable) to write a schema which has multiple columns with the same name,
-- so a list of results are returned for each key.
schemaToTypeMap :: forall k. Rel8able k => k Name -> M.Map String (NonEmpty.NonEmpty TypeInfo)
schemaToTypeMap cols = go . uncurry zip . getConst $
  htabulateA @(Columns (k Name)) $ \field -> 
    case (hfield hspecs field, hfield (toColumns cols) field) of 
      (Spec {..}, Name name) -> Const ([name], [
        TypeInfo { label = labels
                 , isNull = nullableToBool nullity
                 , typeName = info.typeName}])
  where
    go :: [(String, TypeInfo)] -> M.Map String (NonEmpty.NonEmpty TypeInfo)
    go = M.fromListWith (<>) . map (\(name, typeInfo) -> (name, NonEmpty.singleton typeInfo))

-- A checked version of @schemaToTypeMap@, which returns a list of columns with
-- duplicate names if any such columns are present. Otherwise it returns the
-- type map with no duplicates.
checkedSchemaToTypeMap :: Rel8able k
  => k Name
  -> Either (M.Map String (NonEmpty.NonEmpty TypeInfo)) (M.Map String TypeInfo)
checkedSchemaToTypeMap cols =
  let typeMap = schemaToTypeMap cols
      duplicates = M.filter (\col -> length col > 1) typeMap
  in if length duplicates > 0
  then Left duplicates
  else Right (typeMap & M.mapMaybe \case
    a :| [] -> Just a
    _ -> Nothing)


showCreateTable_helper :: String -> M.Map String TypeInfo -> String
showCreateTable_helper name typeMap = "CREATE TABLE " <> show name <> " ("
    ++ L.intercalate "," (fmap go $ M.assocs typeMap)
    ++ "\n);"
  where
    go :: (String, TypeInfo) -> String
    go (name', typeInfo) = "\n    " ++ show name' ++ " " ++ showTypeInfo typeInfo


-- |@'showCreateTable'@ shows an example CREATE TABLE statement for the table.
-- This does not show relationships like primary or foreign keys, but can still
-- be useful to see what types @rel8@ will expect of the underlying database.
--
-- In the event that multiple columns have the same name, this will fail silently. To
-- handle that case, see 'checkedShowCreateTable'
showCreateTable :: Rel8able k => TableSchema (k Name) -> String
showCreateTable schema = showCreateTable_helper schema.name.name $ fmap NonEmpty.head $ schemaToTypeMap schema.columns

-- |@'checkedShowCreateTable'@ shows an example CREATE TABLE statement for the
-- table. This does not show relationships like primary or foreign keys, but can
-- still be useful to see what types rel8 will expect of the underlying database.
--
-- In the event that multiple columns have the same name, this will return a map of
-- names to the labels identifying the column.
checkedShowCreateTable :: Rel8able k => TableSchema (k Name) -> Either (M.Map String (NonEmpty [String])) String
checkedShowCreateTable schema = case checkedSchemaToTypeMap schema.columns of
    Left e -> Left $ (fmap . fmap) (\typ -> typ.label)  e
    Right a -> Right $ showCreateTable_helper schema.name.name a

-- implicit casts are ok as long as they're bidirectional
checkTypeEquality :: CheckEnv -> TypeInfo -> TypeInfo -> Maybe ColumnError
checkTypeEquality env db hs
  | Prelude.and [sameDims, sameMods, toName db == toName hs || castExists]
    = Nothing
  | otherwise
    = Just BidirectionalCastDoesNotExist
  where
    castExists = Prelude.and
      [ (toName db, toName hs) `elem` env.casts
      , (toName hs, toName db) `elem` env.casts
      ]

    sameMods, sameDims :: Bool
    sameMods = db.typeName.modifiers == hs.typeName.modifiers
    sameDims = db.typeName.arrayDepth == hs.typeName.arrayDepth

    toName :: TypeInfo -> String
    toName typeInfo = case typeInfo.typeName.name of
        QualifiedName name _ -> L.dropWhile (== '_') name

-- check types for a single table
compareTypes
    :: CheckEnv
    -> M.Map String (Attribute Result)
    -> M.Map String TypeInfo
    -> [ColumnInfo]
compareTypes env attrMap typeMap = fmap (uncurry go) $ M.assocs (disjointUnion attrMap typeMap)
  where
    go :: String -> These (Attribute Result) TypeInfo -> ColumnInfo
    go name (These a b) = ColumnInfo
        { name = name
        , dbType = Just $ fromAttribute a
        , hsType = Just $ b
        , error = checkTypeEquality env (fromAttribute a) b
        }
    go name (This a) = ColumnInfo
        { name = name
        , dbType = Just $ fromAttribute a
        , hsType = Nothing
        , error =
            if a.attribute.attnotnull
            then Just DbTypeIsNotNullButNotPresentInHsType
            else Nothing
        }
    go name (That b) = ColumnInfo
        { name = name
        , dbType = Nothing
        , hsType = Just $ b
        , error = Just HsTypeIsPresentButNotPresentInDbType
        }

    fromAttribute :: Attribute Result -> TypeInfo
    fromAttribute attr = TypeInfo
        { label = [T.unpack attr.attribute.attname]
        , isNull = not attr.attribute.attnotnull
        , typeName = TypeName
            { name = QualifiedName
                (T.unpack attr.typ.typname)
                (Just $ T.unpack attr.namespace.nspname)
            , modifiers = toModifier
                (T.dropWhile (== '_') attr.typ.typname)
                attr.attribute.atttypmod
            , arrayDepth = fromIntegral attr.attribute.attndims
            }
        }

    toModifier :: Text -> Int64 -> [String]
    toModifier "bpchar" (-1) = []
    toModifier "bpchar" n = [show (n - 4)]
    toModifier "numeric" (-1) = []
    toModifier "numeric" n = [show $ (n - 4) `shiftR` 16, show $ (n - 4) .&. 65535]
    toModifier _ _ = []

    disjointUnion :: Ord k => M.Map k a -> M.Map k b -> M.Map k (These a b)
    disjointUnion = align


-- |@pShowTable@ i's a helper f'unction which takes a grid of text and prints' it'
-- as a table, with padding so that cells are lined in columns, and a bordered
-- header for the first row
pShowTable :: [[Text]] -> Text
pShowTable xs
    = T.intercalate "\n"
    $ addHeaderBorder
    $ fmap (T.intercalate " | ")
    $ L.transpose
    $ zip lengths xs' <&> \(n, column) -> column <&> \cell -> T.justifyLeft n ' ' cell
  where
    addHeaderBorder :: [Text] -> [Text]
    addHeaderBorder [] = []
    addHeaderBorder (a : as) = a : T.replicate (T.length a) "-" : as

    xs' :: [[Text]]
    xs' = L.transpose xs

    lengths :: [Int]
    lengths = fmap (maximum . fmap T.length) $ xs'


pShowErrors :: [TableInfo] -> Text
pShowErrors = T.intercalate "\n\n" . fmap go
  where
    go :: TableInfo -> Text
    go (TableInfo {tableExists, name, columns}) = "Table: " <> T.pack name
        <> if not tableExists then " does not exist\n" else "\n"
        <> pShowTable (["Column Name", "Implied DB type", "Current DB type", "Error"] : (columns <&> \column ->
            [ T.pack $ column.name
            , T.pack $ maybe "" showTypeInfo column.hsType
            , T.pack $ maybe "" showTypeInfo column.dbType
            , T.pack $ maybe "" show column.error
            ]))
    go (DuplicateNames {name, duplicates}) = mconcat
        [ "Table "
        , T.pack (show name)
        , " has multiple columns with the same name. This is an error with the Haskell code generating an impossible schema, rather than an error in your current setup of the database itself. Using 'namesFromLabels' can ensure each column has unique names, which is the easiest way to prevent this, but may require changing names in your database to match the new generated names."
        , pShowTable (["DB name", "Haskell label"] : (M.assocs duplicates <&> \(name', typs) ->
            [ T.pack name'
            , T.intercalate " " $ fmap (\typ -> T.intercalate "/" $ fmap T.pack typ.label) $ NonEmpty.toList typs
            ]))
        ]


data TableInfo
  = TableInfo
    { tableExists :: Bool
    , name :: String
    , columns :: [ColumnInfo]
    }
  | DuplicateNames
    { name :: String
    , duplicates :: M.Map String (NonEmpty.NonEmpty TypeInfo)
    }
  deriving (Show)

data ColumnInfo = ColumnInfo
    { name   :: String
    , hsType :: Maybe TypeInfo
    , dbType :: Maybe TypeInfo
    , error :: Maybe ColumnError
    } deriving (Show)

data ColumnError
    = DbTypeIsNotNullButNotPresentInHsType
    | HsTypeIsPresentButNotPresentInDbType
    | BidirectionalCastDoesNotExist
    deriving (Show)


showTypeInfo :: TypeInfo -> String
showTypeInfo typeInfo = concat
    [ name
    , if Prelude.null modifiers then "" else "(" <> L.intercalate "," modifiers <> ")"
    , concat (replicate (fromIntegral typeInfo.typeName.arrayDepth) "[]")
    , if typeInfo.isNull then "" else " NOT NULL"
    ]
  where
    name = case typeInfo.typeName.name of
        QualifiedName a Nothing -> show (dropWhile (== '_') a)
        QualifiedName a (Just b) -> show b <> "." <> show (dropWhile (== '_') a)

    modifiers :: [String]
    modifiers = typeInfo.typeName.modifiers


verifySchema :: Rel8able k => CheckEnv -> TableSchema (k Name) -> TableInfo
verifySchema env schema = case checkedSchemaToTypeMap schema.columns of
    Left dups -> DuplicateNames schema.name.name dups
    Right typeMap -> go typeMap maybeTable
  where
    maybeTable = M.lookup schema.name.name env.schemaMap
    go typeMap Nothing = TableInfo
        { tableExists = False
        , name = schema.name.name
        , columns = compareTypes env mempty typeMap
        }
    go typeMap (Just attrs) = TableInfo
        { tableExists = True
        , name = schema.name.name
        , columns = compareTypes env (attrsToMap attrs) typeMap
        }


fetchCheckEnv :: HS.Statement () CheckEnv
fetchCheckEnv = fetchSchema <&> \(tbls, casts) -> 
  let tblMap = foldMap (\PGTable {..} -> M.singleton (T.unpack name) columns) tbls
      castMap = map (\Cast {..} -> (T.unpack source.typname, T.unpack target.typname)) $ L.filter (\Cast {context} -> context == "i") casts
  in CheckEnv tblMap castMap
 where
  fetchSchema :: HS.Statement () ([PGTable Result], [Cast Result])
  fetchSchema = run1 $ select $ liftA2 (,) fetchTables fetchCasts


-- |@'SomeTableSchema'@ is used to allow the collection of a variety of different
-- @TableSchema@s under a single type, like:
--
-- @
-- userTable :: TableSchema (User Name)
-- orderTable :: TableSchema (Order Name)
--
-- tables :: [SomeTableSchema]
-- tables = [SomeTableSchema userTable, SomeTable orderTable]
-- @
--
-- This is used by @'schemaErrors'@ to conveniently group every table an
-- application relies on for typechecking the postgresql schemas
-- together in a single batch.
data SomeTableSchema where
    -- The ToExpr constraint isn't used here, but can be used to read from the
    -- SomeTableSchema, which can be useful to combine the type checking with more
    -- thorough value-level checking of the validity of existing rows in the
    -- table.
    SomeTableSchema
        :: (ToExprs (k Expr) (GFromExprs k), Rel8able k)
        => TableSchema (k Name) -> SomeTableSchema

-- |@'getSchemaErrors'@ checks whether the provided schemas have the correct PostgreSQL
-- column names and types to allow reading and writing from their equivalent Haskell
-- types, returning a list of errors if that is not the case. The function does not
-- crash on encountering a bug, instead leaving it to the caller to decide how
-- to respond. A schema is valid if:
--
-- 1. for every existing field, the types match
-- 2. all non-nullable columns are present in the hs type
-- 3. no nonexistent columns are present in the hs type
-- 4. no two columns in the same schema share the same name
--
-- It's still possible for a valid schema to allow invalid data, for instance,
-- if using an ADT, which can introduce restrictions on which values are allowed
-- for the column representing the tag, and introduce restrictions on which
-- columns are non-null depending on the value of the tag. However, if the
-- schema is valid rel8 shouldn't be able to write invalid data to the table.
--
-- However, it is possible for migrations to cause valid data to become invalid
-- in ways not detectable by this function, if the migration code changes the
-- schema correctly but doesn't handle the value-level constraints correctly. So
-- it is a good idea to both read from the tables and check the schema for errors
-- in a transaction during the migration. The former will catch value-level
-- bugs, while the latter will help ensure the schema is set up correctly to
-- be able to insert new data.
--
-- This function does nothing to check that the conflict target of an @Upsert@
-- are valid for the schema, nor can it prevent invalid uses of @unsafeDefault@.
-- However, it should be enough to catch the most likely errors.
getSchemaErrors :: [SomeTableSchema] -> HS.Statement () (Maybe Text)
getSchemaErrors someTables = fmap collectErrors fetchCheckEnv
  where
    collectErrors :: CheckEnv -> Maybe Text
    collectErrors env
        = fmap pShowErrors
        . filterErrors
        . fmap \case
            SomeTableSchema t -> verifySchema env t
        $ someTables

    -- removes each column which is valid for use by rel8, as well as each table
    -- which contains only valid columns
    filterErrors :: [TableInfo] -> Maybe [TableInfo]
    filterErrors tables = case mapMaybe go tables of
        [] -> Nothing
        xs -> Just xs
      where
        go :: TableInfo -> Maybe TableInfo
        go TableInfo {..} = case P.filter (\cd -> isJust cd.error) columns of
            [] -> if tableExists then Nothing else Just $ TableInfo { name , tableExists , columns = [] }
            xs -> Just $ TableInfo { name , tableExists , columns = xs }
        go DuplicateNames {..} = Just (DuplicateNames {..})


