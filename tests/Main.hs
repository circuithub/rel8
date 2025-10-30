{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language CPP #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MonoLocalBinds #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}

{-# language PartialTypeSignatures #-}

module Main
  ( main
  )
where

-- aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap

-- base
import Control.Applicative ( empty, liftA2, liftA3 )
import Control.Exception ( bracket, throwIO )
import Control.Monad ((>=>))
import Data.Bifunctor ( bimap )
import Data.Fixed (Fixed (MkFixed))
import Data.Foldable ( for_ )
import Data.Fixed (Centi)
import Data.Functor (void)
import Data.Int ( Int32, Int64 )
import Data.List ( nub, sort )
import Data.Maybe ( catMaybes )
import Data.Ratio ((%))
import Data.Word (Word32)
import GHC.Generics ( Generic )
import Prelude hiding (truncate)

-- bytestring
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy
import Data.ByteString ( ByteString )

-- case-insensitive
import Data.CaseInsensitive ( mk )

-- containers
import Data.Containers.ListUtils ( nubOrdOn )
import qualified Data.Map.Strict as Map

-- hasql
import Hasql.Connection ( Connection, ConnectionError, acquire, release )
#if MIN_VERSION_hasql(1,9,0)
import qualified Hasql.Connection.Setting
import qualified Hasql.Connection.Setting.Connection
#endif
import Hasql.Session ( sql, run )

-- hasql-transaction
import Hasql.Transaction ( Transaction, condemn, statement )
import qualified Hasql.Transaction as Hasql
import qualified Hasql.Transaction.Sessions as Hasql

-- hedgehog
import Hedgehog ( annotate, failure, property, (===), forAll, cover, diff, evalM, PropertyT, TestT, test, Gen )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- iproute
import qualified Data.IP

-- mmorph
import Control.Monad.Morph ( hoist )

-- rel8
import Rel8 ( Result )
import qualified Rel8
import qualified Rel8.Generic.Rel8able.Test as Rel8able
import qualified Rel8.Table.Verify as Verify

-- scientific
import Data.Scientific ( Scientific )

-- tasty
import Test.Tasty

-- tasty-hedgehog
import Test.Tasty.Hedgehog ( testProperty )

-- text
import Data.Text ( Text, unpack )
import qualified Data.Text as T
import qualified Data.Text.Lazy
import Data.Text.Encoding ( decodeUtf8 )

-- time
import Data.Time

-- transformers
import Control.Monad.Trans.Class ( lift )

-- tmp-postgres
import qualified Database.Postgres.Temp as TmpPostgres

-- uuid
import qualified Data.UUID

-- vector
import qualified Data.Vector as Vector


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests =
  withResource startTestDatabase stopTestDatabase \getTestDatabase ->
  testGroup "rel8"
    [ testSelectTestTable getTestDatabase
    , testWithStatement getTestDatabase
    , testWhere_ getTestDatabase
    , testFilter getTestDatabase
    , testLimit getTestDatabase
    , testUnion getTestDatabase
    , testDistinct getTestDatabase
    , testExists getTestDatabase
    , testOptional getTestDatabase
    , testAnd getTestDatabase
    , testOr getTestDatabase
    , testNot getTestDatabase
    , testBool getTestDatabase
    , testAp getTestDatabase
    , testDBType getTestDatabase
    , testDBEq getTestDatabase
    , testTableEquality getTestDatabase
    , testFromRational getTestDatabase
    , testCatMaybeTable getTestDatabase
    , testCatMaybe getTestDatabase
    , testMaybeTable getTestDatabase
    , testAggregateMaybeTable getTestDatabase
    , testNestedTables getTestDatabase
    , testMaybeTableApplicative getTestDatabase
    , testLogicalFixities getTestDatabase
    , testUpdate getTestDatabase
    , testDelete getTestDatabase
    , testUpsert getTestDatabase
    , testSelectNestedPairs getTestDatabase
    , testSelectArray getTestDatabase
    , testNestedMaybeTable getTestDatabase
    , testEvaluate getTestDatabase
    , testShowCreateTable getTestDatabase
    ]
  where
    startTestDatabase = do
      db <- TmpPostgres.start >>= either throwIO return

      bracket (either (error . show) return =<< acquireFromConnectionString (TmpPostgres.toConnectionString db)) release \conn -> void do
        flip run conn do
          sql "CREATE EXTENSION citext"
          sql "CREATE TABLE test_table ( column1 text not null, column2 bool not null )"
          sql "CREATE TABLE unique_table ( \"key\" text not null unique, \"value\" text not null )"
          sql "CREATE SEQUENCE test_seq"
          sql "CREATE TYPE composite AS (\"bool\" bool, \"char\" text, \"array\" int4[])"

      return db

    stopTestDatabase = TmpPostgres.stop


connect :: TmpPostgres.DB -> IO Connection
connect = acquireFromConnectionString . TmpPostgres.toConnectionString >=> either (maybe empty (fail . unpack . decodeUtf8)) pure

acquireFromConnectionString :: ByteString -> IO (Either ConnectionError Connection)
acquireFromConnectionString connectionString =
#if MIN_VERSION_hasql(1,9,0)
  acquire 
    [ Hasql.Connection.Setting.connection . Hasql.Connection.Setting.Connection.string . decodeUtf8 $ connectionString
    ]
#else
  acquire connectionString
#endif

testShowCreateTable :: IO TmpPostgres.DB -> TestTree
testShowCreateTable getTestDatabase = testGroup "CREATE TABLE"
  [ testTypeChecker "tableTest" Rel8able.tableTest Rel8able.genTableTest getTestDatabase
  , testTypeChecker "tablePair" Rel8able.tablePair Rel8able.genTablePair getTestDatabase
  , testTypeChecker "tableMaybe" Rel8able.tableMaybe Rel8able.genTableMaybe getTestDatabase
  , testTypeChecker "tableEither" Rel8able.tableEither Rel8able.genTableEither getTestDatabase
  , testTypeChecker "tableThese" Rel8able.tableThese Rel8able.genTableThese getTestDatabase
  , testTypeChecker "tableList" Rel8able.tableList Rel8able.genTableList getTestDatabase
  , testTypeChecker "tableNest" Rel8able.tableNest Rel8able.genTableNest getTestDatabase
  , testTypeChecker "nonRecord" Rel8able.nonRecord Rel8able.genNonRecord getTestDatabase
  , testTypeChecker "tableProduct" Rel8able.tableProduct Rel8able.genTableProduct getTestDatabase
  , testTypeChecker "tableType" Rel8able.tableType Rel8able.genTableType getTestDatabase
  , testWrongTable getTestDatabase
  , testDuplicateTable getTestDatabase
  , testCharMismatch getTestDatabase
  , testNumericMismatch getTestDatabase
  ]
  where
    -- confirms that the type checker works correctly for numeric modifiers
    testNumericMismatch = databasePropertyTest "numeric mismatch" \transaction -> transaction do
      lift $ Hasql.sql $ "create table \"tableNumeric\" ( foo numeric(1000, 4) not null );"
      typeErrors <- lift $ statement () $ Verify.getSchemaErrors
        [Verify.SomeTableSchema Rel8able.tableNumeric]
      case typeErrors of
        Nothing -> failure
        Just _ -> pure ()
      lift $ Hasql.sql $ "alter table \"tableNumeric\" alter column foo set data type numeric(1000, 2);"
      typeErrors <- lift $ statement () $ Verify.getSchemaErrors
        [Verify.SomeTableSchema Rel8able.tableNumeric]
      case typeErrors of
        Nothing -> pure ()
        Just _ -> failure

    -- tests that the type checker works correctly for bpchar modifiers
    testCharMismatch = databasePropertyTest "bpchar mismatch" \transaction -> transaction do
      lift $ Hasql.sql $ "create table \"tableChar\" ( foo bpchar(2) not null );"
      typeErrors <- lift $ statement () $ Verify.getSchemaErrors
        [Verify.SomeTableSchema Rel8able.tableChar]
      case typeErrors of
        Nothing -> failure
        Just _ -> pure ()
      lift $ Hasql.sql $ "alter table \"tableChar\" alter column foo set data type bpchar(1);"
      typeErrors <- lift $ statement () $ Verify.getSchemaErrors
        [Verify.SomeTableSchema Rel8able.tableChar]
      case typeErrors of
        Nothing -> pure ()
        Just a -> do
            annotate (unpack a)
            failure

    -- confirms that the type checker fails when no type errors are present in a
    -- table with duplicate column names
    testDuplicateTable = databasePropertyTest "duplicate columns" \transaction -> transaction do
      lift $ Hasql.sql $ B.pack $ Verify.showCreateTable Rel8able.tableDuplicate
      typeErrors <- lift $ statement () $ Verify.getSchemaErrors
        [Verify.SomeTableSchema Rel8able.tableDuplicate]
      case typeErrors of
        Nothing -> failure
        Just _ -> pure ()

    -- confirms that the type checker fails if the types mismatch
    testWrongTable = databasePropertyTest "type mismatch" \transaction -> transaction do
      lift $ Hasql.sql $ B.pack $ Verify.showCreateTable Rel8able.tableType
      typeErrors <- lift $ statement () $ Verify.getSchemaErrors
        [Verify.SomeTableSchema Rel8able.badTableType]
      case typeErrors of
        Nothing -> failure
        Just _ -> pure ()

    testTypeChecker ::
      ( Show (k Result), Rel8.Rel8able k, Rel8.Selects (k Rel8.Name) (k Rel8.Expr)
      , Rel8.Serializable (k Rel8.Expr) (k Rel8.Result))
      => TestName -> Rel8.TableSchema (k Rel8.Name) -> Gen (k Result) -> IO TmpPostgres.DB -> TestTree
    testTypeChecker testName tableSchema genRows = databasePropertyTest testName \transaction -> do
      rows <- forAll $ Gen.list (Range.linear 0 10) genRows

      transaction do
        lift $ Hasql.sql $ B.pack $ Verify.showCreateTable tableSchema
        typeErrors <- lift $ statement () $ Verify.getSchemaErrors [Verify.SomeTableSchema tableSchema]
        case typeErrors of
          Nothing -> pure ()
          Just typ -> do
            annotate (unpack typ)
            failure

        selected <- lift do
          statement () $ Rel8.run_ $ Rel8.insert Rel8.Insert
            { into = tableSchema
            , rows = Rel8.values $ map Rel8.lit rows
            , onConflict = Rel8.DoNothing Nothing
            , returning = Rel8.NoReturning
            }
          statement () $ Rel8.run $ Rel8.select do
            Rel8.each tableSchema

        -- not every type we use this with has an ord instance, and we're
        -- primarily checking the type checker here, not the parser/printer,
        -- so we this is only here as one additional check
        length selected === length rows


databasePropertyTest
  :: TestName
  -> ((TestT Transaction () -> PropertyT IO ()) -> PropertyT IO ())
  -> IO TmpPostgres.DB -> TestTree
databasePropertyTest testName f getTestDatabase =
  withResource (connect =<< getTestDatabase) release $ \c ->
  testProperty testName $ property do
    connection <- lift c
    f $ test . hoist \m -> do
      e <- run (Hasql.transaction Hasql.Serializable Hasql.Write (m <* condemn)) connection
      either throwIO pure e


data TestTable f = TestTable
  { testTableColumn1 :: Rel8.Column f Text
  , testTableColumn2 :: Rel8.Column f Bool
  }
  deriving stock Generic
  deriving anyclass Rel8.Rel8able


deriving stock instance Eq (TestTable Result)
deriving stock instance Ord (TestTable Result)
deriving stock instance Show (TestTable Result)


testTableSchema :: Rel8.TableSchema (TestTable Rel8.Name)
testTableSchema =
  Rel8.TableSchema
    { name = "test_table"
    , columns = TestTable
        { testTableColumn1 = "column1"
        , testTableColumn2 = "column2"
        }
    }


testSelectTestTable :: IO TmpPostgres.DB -> TestTree
testSelectTestTable = databasePropertyTest "Can SELECT TestTable" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  transaction do
    selected <- lift do
      statement () $ Rel8.run_ $ Rel8.insert Rel8.Insert
        { into = testTableSchema
        , rows = Rel8.values $ map Rel8.lit rows
        , onConflict = Rel8.DoNothing Nothing
        , returning = Rel8.NoReturning
        }

      statement () $ Rel8.run $ Rel8.select do
        Rel8.each testTableSchema

    sort selected === sort rows

    cover 1 "Empty" $ null rows
    cover 1 "Singleton" $ null $ drop 1 rows
    cover 1 ">1 row" $ not $ null $ drop 1 rows


testWhere_ :: IO TmpPostgres.DB -> TestTree
testWhere_ = databasePropertyTest "WHERE (Rel8.where_)" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 1 10) genTestTable

  magicBool <- forAll Gen.bool

  let expected = filter (\t -> testTableColumn2 t == magicBool) rows

  transaction do
    selected <- lift do
      statement () $ Rel8.run $ Rel8.select do
        t <- Rel8.values $ Rel8.lit <$> rows
        Rel8.where_ $ testTableColumn2 t Rel8.==. Rel8.lit magicBool
        return t

    sort selected === sort expected

    cover 1 "No results" $ null expected
    cover 1 "Some results" $ not $ null expected
    cover 1 "All results" $ expected == rows


testFilter :: IO TmpPostgres.DB -> TestTree
testFilter = databasePropertyTest "filter" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 1 10) genTestTable

  transaction do
    let expected = filter testTableColumn2 rows

    selected <- lift do
      statement () $ Rel8.run $ Rel8.select do
        Rel8.filter testTableColumn2 =<< Rel8.values (Rel8.lit <$> rows)

    sort selected === sort expected

    cover 1 "No results" $ null expected
    cover 1 "Some results" $ not $ null expected
    cover 1 "All results" $ expected == rows


testLimit :: IO TmpPostgres.DB -> TestTree
testLimit = databasePropertyTest "LIMIT (Rel8.limit)" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 1 10) genTestTable

  n <- forAll $ Gen.integral (Range.linear 0 10)

  transaction do
    selected <- lift do
      statement () $ Rel8.run $ Rel8.select do
        Rel8.limit n $ Rel8.values (Rel8.lit <$> rows)

    diff (length selected) (<=) (fromIntegral n)

    for_ selected \row ->
      diff row elem rows

    cover 1 "n == 0" $ n == 0
    cover 1 "n < length rows" $ fromIntegral n < length rows
    cover 1 "n == length rows" $ fromIntegral n == length rows
    cover 1 "n >= length rows" $ fromIntegral n >= length rows


testUnion :: IO TmpPostgres.DB -> TestTree
testUnion = databasePropertyTest "UNION (Rel8.union)" \transaction -> evalM do
  left <- forAll $ Gen.list (Range.linear 0 10) genTestTable
  right <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  transaction do
    selected <- lift do
      statement () $ Rel8.run $ Rel8.select do
        Rel8.values (Rel8.lit <$> nub left) `Rel8.union` Rel8.values (Rel8.lit <$> nub right)

    sort selected === sort (nub (left ++ right))


testDistinct :: IO TmpPostgres.DB -> TestTree
testDistinct = databasePropertyTest "DISTINCT (Rel8.distinct)" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  transaction do
    selected <- lift do
      statement () $ Rel8.run $ Rel8.select do
        Rel8.distinct do
          Rel8.values (Rel8.lit <$> rows)

    sort selected === nub (sort rows)

    cover 1 "Empty" $ null rows
    cover 1 "Duplicates" $ not (null rows) && rows /= nub rows
    cover 1 "No duplicates" $ not (null rows) && rows == nub rows


testExists :: IO TmpPostgres.DB -> TestTree
testExists = databasePropertyTest "EXISTS (Rel8.exists)" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  transaction do
    exists <- lift do
      statement () $ Rel8.run1 $ Rel8.select do
        Rel8.exists $ Rel8.values $ Rel8.lit <$> rows

    case rows of
      [] -> exists === False
      _ -> exists === True


testOptional :: IO TmpPostgres.DB -> TestTree
testOptional = databasePropertyTest "Rel8.optional" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  transaction do
    selected <- lift do
      statement () $ Rel8.run $ Rel8.select do
        Rel8.optional $ Rel8.values (Rel8.lit <$> rows)

    case rows of
      [] -> selected === [Nothing]
      _  -> sort selected === fmap Just (sort rows)


testAnd :: IO TmpPostgres.DB -> TestTree
testAnd = databasePropertyTest "AND (&&.)" \transaction -> do
  (x, y) <- forAll $ liftA2 (,) Gen.bool Gen.bool

  transaction do
    result <- lift do
      statement () $ Rel8.run1 $ Rel8.select do
        pure $ Rel8.lit x Rel8.&&. Rel8.lit y

    result === (x && y)


testOr :: IO TmpPostgres.DB -> TestTree
testOr = databasePropertyTest "OR (||.)" \transaction -> do
  (x, y) <- forAll $ liftA2 (,) Gen.bool Gen.bool

  transaction do
    result <- lift do
      statement () $ Rel8.run1 $ Rel8.select $ pure $
        Rel8.lit x Rel8.||. Rel8.lit y

    result === (x || y)


testLogicalFixities :: IO TmpPostgres.DB -> TestTree
testLogicalFixities = databasePropertyTest "Logical operator fixities" \transaction -> do
  (u, v, w, x) <- forAll $ (,,,) <$> Gen.bool <*> Gen.bool <*> Gen.bool <*> Gen.bool

  transaction do
    result <- lift do
      statement () $ Rel8.run1 $ Rel8.select do
        pure $ Rel8.lit u Rel8.||. Rel8.lit v Rel8.&&. Rel8.lit w Rel8.==. Rel8.lit x

    result === (u || v && w == x)


testNot :: IO TmpPostgres.DB -> TestTree
testNot = databasePropertyTest "NOT (not_)" \transaction -> do
  x <- forAll Gen.bool

  transaction do
    result <- lift do
      statement () $ Rel8.run1 $ Rel8.select do
        pure $ Rel8.not_ $ Rel8.lit x

    result === not x


testBool :: IO TmpPostgres.DB -> TestTree
testBool = databasePropertyTest "ifThenElse_" \transaction -> do
  (x, y, z) <- forAll $ liftA3 (,,) Gen.bool Gen.bool Gen.bool

  transaction do
    result <- lift do
      statement () $ Rel8.run1 $ Rel8.select do
        pure $ Rel8.bool (Rel8.lit z) (Rel8.lit y) (Rel8.lit x)

    result === if x then y else z


testAp :: IO TmpPostgres.DB -> TestTree
testAp = databasePropertyTest "Cartesian product (<*>)" \transaction -> do
  (rows1, rows2) <- forAll $
    liftA2 (,)
      (Gen.list (Range.linear 1 10) genTestTable)
      (Gen.list (Range.linear 1 10) genTestTable)

  transaction do
    result <- lift do
      statement () $ Rel8.run $ Rel8.select do
        liftA2 (,) (Rel8.values (Rel8.lit <$> rows1)) (Rel8.values (Rel8.lit <$> rows2))

    sort result === sort (liftA2 (,) rows1 rows2)


data Composite = Composite
  { bool :: !Bool
  , char :: !Text
  , array :: ![Int32]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Rel8.DBType) via Rel8.Composite Composite


instance Rel8.DBComposite Composite where
  compositeTypeName = "composite"
  compositeFields = Rel8.namesFromLabels


testDBType :: IO TmpPostgres.DB -> TestTree
testDBType getTestDatabase = testGroup "DBType instances"
  [ dbTypeTest "Bool" Gen.bool
  , dbTypeTest "ByteString" $ Gen.bytes (Range.linear 0 128)
  , dbTypeTest "CalendarDiffTime" genCalendarDiffTime
  , dbTypeTest "Char" Gen.unicode
  , dbTypeTest "CI Lazy Text" $ mk . Data.Text.Lazy.fromStrict <$> genText
  , dbTypeTest "CI Text" $ mk <$> genText
  , dbTypeTest "Composite" genComposite
  , dbTypeTest "Day" genDay
  , dbTypeTest "Double" $ (/ 10) . fromIntegral @Int @Double <$> Gen.integral (Range.linear (-100) 100)
  , dbTypeTest "Fixed" $ toEnum @Centi <$> Gen.integral (Range.linear (-10000) 10000)
  , dbTypeTest "Float" $ (/ 10) . fromIntegral @Int @Float <$> Gen.integral (Range.linear (-100) 100)
  , dbTypeTest "Int32" $ Gen.integral @_ @Int32 Range.linearBounded
  , dbTypeTest "Int64" $ Gen.integral @_ @Int64 Range.linearBounded
  , dbTypeTest "Lazy ByteString" $ Data.ByteString.Lazy.fromStrict <$> Gen.bytes (Range.linear 0 128)
  , dbTypeTest "Lazy Text" $ Data.Text.Lazy.fromStrict <$> genText
  , dbTypeTest "LocalTime" genLocalTime
  , dbTypeTest "Scientific" $ genScientific
  , dbTypeTest "Text" genText
  , dbTypeTest "TimeOfDay" genTimeOfDay
  , dbTypeTest "UTCTime" $ UTCTime <$> genDay <*> genDiffTime
  , dbTypeTest "UUID" $ Data.UUID.fromWords <$> genWord32 <*> genWord32 <*> genWord32 <*> genWord32
  , dbTypeTest "INet" genIPRange
  , dbTypeTest "Value" genValue
  , dbTypeTest "JSONEncoded" genJSONEncoded
  , dbTypeTest "JSONBEncoded" genJSONBEncoded
  ]

  where
    dbTypeTest :: (Eq a, Show a, Rel8.DBType a, Rel8.ToExprs (Rel8.Expr a) a) => TestName -> Gen a -> TestTree
    dbTypeTest name generator = testGroup name
      [ databasePropertyTest name (t generator) getTestDatabase
      , databasePropertyTest ("Maybe " <> name) (t (Gen.maybe generator)) getTestDatabase
      ]

    t :: forall a. (Eq a, Show a, Rel8.Sql Rel8.DBType a, Rel8.ToExprs (Rel8.Expr a) a)
      => Gen a
      -> (TestT Transaction () -> PropertyT IO ())
      -> PropertyT IO ()
    t generator transaction = do
      x <- forAll generator
      y <- forAll generator
      xss <- forAll $ Gen.list (Range.linear 0 10) (Gen.list (Range.linear 0 10) generator)
      xsss <- forAll $ Gen.list (Range.linear 0 10) (Gen.list (Range.linear 0 10) (Gen.list (Range.linear 0 10) generator))

      transaction do
        res <- lift do
          statement () $ Rel8.run1 $ Rel8.select do
            pure (Rel8.litExpr x)
        diff res (==) x
        res' <- lift do
          statement () $ Rel8.run1 $ Rel8.select $ Rel8.many $ Rel8.many do
            Rel8.values [Rel8.litExpr x, Rel8.litExpr y]
        diff res' (==) [[x, y]]
        res3 <- lift do
          statement () $ Rel8.run1 $ Rel8.select $ Rel8.many $ Rel8.many $ Rel8.many do
            Rel8.values [Rel8.litExpr x, Rel8.litExpr y]
        diff res3 (==) [[[x, y]]]
        res'' <- lift do
          statement () $ Rel8.run $ Rel8.select do
            xs <- Rel8.catListTable (Rel8.listTable [Rel8.listTable [Rel8.litExpr x, Rel8.litExpr y]])
            Rel8.catListTable xs
        diff res'' (==) [x, y]
        res''' <- lift do
          statement () $ Rel8.run $ Rel8.select do
            xss' <- Rel8.catListTable (Rel8.listTable [Rel8.listTable [Rel8.listTable [Rel8.litExpr x, Rel8.litExpr y]]])
            xs <- Rel8.catListTable xss'
            Rel8.catListTable xs
        diff res''' (==) [x, y]
        res'''' <- lift do
          statement () $ Rel8.run1 $ Rel8.select $
            Rel8.aggregate Rel8.listCatExpr $
              Rel8.values $ map Rel8.litExpr xss
        diff res'''' (==) (concat xss)
        res''''' <- lift do
          statement () $ Rel8.run1 $ Rel8.select $
            Rel8.aggregate Rel8.listCatExpr $
              Rel8.values $ map Rel8.litExpr xsss
        diff res''''' (==) (concat xsss)

      transaction do
        res <- lift do
          statement x $ Rel8.prepared Rel8.run1 $
            Rel8.select @(Rel8.Expr _) .
            pure
        diff res (==) x

        res' <- lift do
          statement [x, y] $ Rel8.prepared Rel8.run1 $
            Rel8.select @(Rel8.ListTable Rel8.Expr (Rel8.Expr _)) .
            Rel8.many . Rel8.catListTable
        diff res' (==) [x, y]

        res'' <- lift do
          statement [[x, y]] $ Rel8.prepared Rel8.run1 $
            Rel8.select @(Rel8.ListTable Rel8.Expr (Rel8.ListTable Rel8.Expr (Rel8.Expr _))) .
            Rel8.many . Rel8.many . (Rel8.catListTable >=> Rel8.catListTable)
        diff res'' (==) [[x, y]]

        res''' <- lift do
          statement [[[x, y]]] $ Rel8.prepared Rel8.run1 $
            Rel8.select @(Rel8.ListTable Rel8.Expr (Rel8.ListTable Rel8.Expr (Rel8.ListTable Rel8.Expr (Rel8.Expr _)))) .
            Rel8.many . Rel8.many . Rel8.many . (Rel8.catListTable >=> Rel8.catListTable >=> Rel8.catListTable)
        diff res''' (==) [[[x, y]]]

    genScientific :: Gen Scientific
    genScientific = (/ 10) . fromIntegral @Int @Scientific <$> Gen.integral (Range.linear (-100) 100)

    genComposite :: Gen Composite
    genComposite = do
      bool <- Gen.bool
      char <- genText
      array <- Gen.list (Range.linear 0 10) (Gen.int32 (Range.linear (-10000) 10000))
      pure Composite {..}

    genDay :: Gen Day
    genDay = do
      year <- Gen.integral (Range.linear 1970 3000)
      month <- Gen.integral (Range.linear 1 12)
      day <- Gen.integral (Range.linear 1 31)
      Gen.just $ pure $ fromGregorianValid year month day

    genCalendarDiffTime :: Gen CalendarDiffTime
    genCalendarDiffTime = do
      -- hardcoded to 0 because Hasql's 'interval' decoder needs to return a
      -- CalendarDiffTime for this to be properly round-trippable
      months <- pure 0 -- Gen.integral (Range.linear 0 120)
      diffTime <- secondsToNominalDiffTime . MkFixed . (* 1000000) <$> Gen.integral (Range.linear 0 2147483647999999)
      pure $ CalendarDiffTime months diffTime

    genDiffTime :: Gen DiffTime
    genDiffTime = secondsToDiffTime <$> Gen.integral (Range.linear 0 86401)

    genTimeOfDay :: Gen TimeOfDay
    genTimeOfDay = do
      hour <- Gen.integral (Range.linear 0 23)
      minute <- Gen.integral (Range.linear 0 59)
      sec <- fromIntegral @Int <$> Gen.integral (Range.linear 0 59)
      Gen.just $ pure $ makeTimeOfDayValid hour minute sec

    genLocalTime = LocalTime <$> genDay <*> genTimeOfDay

    genWord32 :: Gen Word32
    genWord32 = Gen.integral Range.linearBounded

    genIPRange :: Gen (Data.IP.IPRange)
    genIPRange =
      Gen.choice
        [ Data.IP.IPv4Range <$> (Data.IP.makeAddrRange <$> genIPv4 <*> genIP4Mask)
        , Data.IP.IPv6Range <$> (Data.IP.makeAddrRange <$> genIPv6 <*> genIP6Mask)
        ]
      where
        genIP4Mask :: Gen Int
        genIP4Mask = Gen.integral (Range.linearFrom 0 0 32)

        genIPv4 :: Gen Data.IP.IPv4
        genIPv4 = Data.IP.toIPv4w <$> genWord32

        genIP6Mask :: Gen Int
        genIP6Mask = Gen.integral (Range.linearFrom 0 0 128)

        genIPv6 :: Gen (Data.IP.IPv6)
        genIPv6 = Data.IP.toIPv6w <$> ((,,,) <$> genWord32 <*> genWord32 <*> genWord32 <*> genWord32)

    genKey :: Gen Aeson.Key
    genKey = Aeson.Key.fromText <$> genText

    genValue :: Gen Aeson.Value
    genValue = Gen.recursive Gen.choice
     [ pure Aeson.Null
     , Aeson.Bool <$> Gen.bool
     , Aeson.Number <$> genScientific
     , Aeson.String <$> genText
     ]
     [ Aeson.Object . Aeson.KeyMap.fromMap <$> Gen.map (Range.linear 0 10) ((,) <$> genKey <*> genValue)
     , Aeson.Array . Vector.fromList <$> Gen.list (Range.linear 0 10) genValue
     ]

    genJSONEncoded = Rel8.JSONEncoded <$> genValue
    genJSONBEncoded = Rel8.JSONBEncoded <$> genValue


testDBEq :: IO TmpPostgres.DB -> TestTree
testDBEq getTestDatabase = testGroup "DBEq instances"
  [ dbEqTest "Bool" Gen.bool
  , dbEqTest "Int32" $ Gen.integral @_ @Int32 Range.linearBounded
  , dbEqTest "Int64" $ Gen.integral @_ @Int64 Range.linearBounded
  , dbEqTest "Text" $ genText
  ]

  where
    dbEqTest :: (Eq a, Show a, Rel8.DBEq a) => TestName -> Gen a -> TestTree
    dbEqTest name generator = testGroup name
      [ databasePropertyTest name (t generator) getTestDatabase
      , databasePropertyTest ("Maybe " <> name) (t (Gen.maybe generator)) getTestDatabase
      ]

    t :: forall a. (Eq a, Show a, Rel8.Sql Rel8.DBEq a)
      => Gen a
      -> (TestT Transaction () -> PropertyT IO ())
      -> PropertyT IO ()
    t generator transaction = do
      (x, y) <- forAll (liftA2 (,) generator generator)

      transaction do
        res <- lift do
          statement () $ Rel8.run1 $ Rel8.select do
            pure $ Rel8.litExpr x Rel8.==. Rel8.litExpr y
        res === (x == y)


genText :: Gen Text
genText = removeNull <$> Gen.text (Range.linear 0 10) Gen.unicode
  where
    -- | Postgres doesn't support the NULL character (not to be confused with a NULL value) inside strings.
    removeNull :: Text -> Text
    removeNull = T.filter (/= '\0')



testTableEquality :: IO TmpPostgres.DB -> TestTree
testTableEquality = databasePropertyTest "TestTable equality" \transaction -> do
   (x, y) <- forAll $ liftA2 (,) genTestTable genTestTable

   transaction do
     eq <- lift do
       statement () $ Rel8.run1 $ Rel8.select do
         pure $ Rel8.lit x Rel8.==: Rel8.lit y

     eq === (x == y)


testFromRational :: IO TmpPostgres.DB -> TestTree
testFromRational = databasePropertyTest "fromRational" \transaction -> do
  numerator <- forAll $ Gen.int64 Range.linearBounded
  denominator <- forAll $ Gen.int64 $ Range.linear 1 maxBound

  let
    rational = toInteger numerator % toInteger denominator
    double = fromRational @Double rational

  transaction do
    result <- lift do
      statement () $ Rel8.run1 $ Rel8.select do
        pure $ fromRational rational
    diff result (~=) double
  where
    wholeDigits x = fromIntegral $ length $ show $ round @_ @Integer x
    -- A Double gives us between 15-17 decimal digits of precision.
    -- It's tempting to say that two numbers are equal if they differ by less than 1e15.
    -- But this doesn't hold.
    -- The precision is split between the whole numer part and the decimal part of the number.
    -- For instance, a number between 10 and 99 only has around 13 digits of precision in its decimal part.
    -- Postgres and Haskell show differing amounts of digits in these cases,
    a ~= b = abs (a - b) < 10 ** (-15 + wholeDigits a)
    infix 4 ~=


testCatMaybeTable :: IO TmpPostgres.DB -> TestTree
testCatMaybeTable = databasePropertyTest "catMaybeTable" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  transaction do
    selected <- lift do
      statement () $ Rel8.run $ Rel8.select do
        testTable <- Rel8.values $ Rel8.lit <$> rows
        Rel8.catMaybeTable $ Rel8.bool Rel8.nothingTable (pure testTable) (testTableColumn2 testTable)

    sort selected === sort (filter testTableColumn2 rows)


testCatMaybe :: IO TmpPostgres.DB -> TestTree
testCatMaybe = databasePropertyTest "catMaybe" \transaction -> evalM do
  rows <- forAll $ Gen.list (Range.linear 0 10) $ Gen.maybe Gen.bool

  transaction do
    selected <- lift do
      statement () $ Rel8.run $ Rel8.select do
        Rel8.catNull =<< Rel8.values (map Rel8.lit rows)

    sort selected === sort (catMaybes rows)


testMaybeTable :: IO TmpPostgres.DB -> TestTree
testMaybeTable = databasePropertyTest "maybeTable" \transaction -> evalM do
  (rows, def) <- forAll $ liftA2 (,) (Gen.list (Range.linear 0 10) genTestTable) genTestTable

  transaction do
    selected <- lift do
      statement () $ Rel8.run $ Rel8.select do
        Rel8.maybeTable (Rel8.lit def) id <$> Rel8.optional (Rel8.values (Rel8.lit <$> rows))

    case rows of
      [] -> selected === [def]
      _ -> sort selected === sort rows


testAggregateMaybeTable :: IO TmpPostgres.DB -> TestTree
testAggregateMaybeTable = databasePropertyTest "aggregateMaybeTable" \transaction -> evalM do
  rows <- forAll $ Gen.list (Range.linear 0 10) (Gen.maybe (Gen.int64 (Range.linear 0 10)))

  let
    aggregate = go 0 False False
      where
        go !n nothing _ (Just a : as) = go (n + a) nothing True as
        go n _ just (Nothing : as) = go n True just as
        go _ False False [] = []
        go _ True False [] = [Nothing]
        go n False True [] = [Just n]
        go n True True [] = [Nothing, Just n]

  transaction do
    selected <- lift do
      statement () $ Rel8.run $ Rel8.select do
        Rel8.aggregate1 (Rel8.aggregateMaybeTable Rel8.sum) $ Rel8.values (Rel8.lit <$> rows)

    sort selected === aggregate rows


data TwoTestTables f =
  TwoTestTables
    { testTable1 :: TestTable f
    , testTable2 :: TestTable f
    }
  deriving stock Generic
  deriving anyclass Rel8.Rel8able


deriving stock instance Eq (TwoTestTables Result)
deriving stock instance Ord (TwoTestTables Result)
deriving stock instance Show (TwoTestTables Result)


testNestedTables :: IO TmpPostgres.DB -> TestTree
testNestedTables = databasePropertyTest "Nested TestTables" \transaction -> evalM do
  rows <- forAll do
    Gen.list (Range.linear 0 10) $
      liftA2 TwoTestTables genTestTable genTestTable

  transaction do
    selected <- lift do
      statement () $ Rel8.run $ Rel8.select do
        Rel8.values (Rel8.lit <$> rows)

    sort selected === sort rows


testMaybeTableApplicative :: IO TmpPostgres.DB -> TestTree
testMaybeTableApplicative = databasePropertyTest "MaybeTable (<*>)" \transaction -> evalM do
  rows1 <- genRows
  rows2 <- genRows

  transaction do
    selected <- lift do
      statement () $ Rel8.run $ Rel8.select do
        as <- Rel8.optional (Rel8.values (Rel8.lit <$> rows1))
        bs <- Rel8.optional (Rel8.values (Rel8.lit <$> rows2))
        pure $ liftA2 (,) as bs

    case (rows1, rows2) of
      ([], []) -> selected === [Nothing]
      ([], bs) -> selected === (Nothing <$ bs)
      (as, []) -> selected === (Nothing <$ as)
      (as, bs) -> sort selected === sort (Just <$> liftA2 (,) as bs)
  where
    genRows :: PropertyT IO [TestTable Result]
    genRows = forAll do
      Gen.list (Range.linear 0 10) $ liftA2 TestTable genText (pure True)


genTestTable :: Gen (TestTable Result)
genTestTable = do
  testTableColumn1 <- Gen.text (Range.linear 0 5) Gen.alphaNum
  testTableColumn2 <- Gen.bool
  return TestTable{..}


testUpdate :: IO TmpPostgres.DB -> TestTree
testUpdate = databasePropertyTest "Can UPDATE TestTable" \transaction -> do
  rows <- forAll $ Gen.map (Range.linear 0 5) $ liftA2 (,) genTestTable genTestTable

  transaction do
    selected <- lift do
      statement () $ Rel8.run_ $ Rel8.insert Rel8.Insert
        { into = testTableSchema
        , rows = Rel8.values $ map Rel8.lit $ Map.keys rows
        , onConflict = Rel8.DoNothing Nothing
        , returning = Rel8.NoReturning
        }

      statement () $ Rel8.run_ $ Rel8.update Rel8.Update
        { target = testTableSchema
        , from = pure ()
        , set = \_ r ->
            let updates = map (bimap Rel8.lit Rel8.lit) $ Map.toList rows
            in
            foldl
              ( \e (x, y) ->
                  Rel8.bool
                    e
                    y
                    ( testTableColumn1 r Rel8.==. testTableColumn1 x Rel8.&&.
                      testTableColumn2 r Rel8.==. testTableColumn2 x
                    )
              )
              r
              updates
        , updateWhere = \_ _ -> Rel8.lit True
        , returning = Rel8.NoReturning
        }

      statement () $ Rel8.run $ Rel8.select do
        Rel8.each testTableSchema

    sort selected === sort (Map.elems rows)

    cover 1 "Empty" $ null rows
    cover 1 "Singleton" $ null $ drop 1 $ Map.keys rows
    cover 1 ">1 row" $ not $ null $ drop 1 $ Map.keys rows


testDelete :: IO TmpPostgres.DB -> TestTree
testDelete = databasePropertyTest "Can DELETE TestTable" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 5) genTestTable

  transaction do
    (deleted, selected) <- lift do
      statement () $ Rel8.run_ $ Rel8.insert Rel8.Insert
        { into = testTableSchema
        , rows = Rel8.values $ map Rel8.lit rows
        , onConflict = Rel8.DoNothing Nothing
        , returning = Rel8.NoReturning
        }

      deleted <- statement () $ Rel8.run $ Rel8.delete Rel8.Delete
          { from = testTableSchema
          , using = pure ()
          , deleteWhere = const testTableColumn2
          , returning = Rel8.Returning id
          }

      selected <- statement () $ Rel8.run $ Rel8.select do
        Rel8.each testTableSchema

      pure (deleted, selected)

    sort (deleted <> selected) === sort rows


testWithStatement :: IO TmpPostgres.DB -> TestTree
testWithStatement genTestDatabase =
  testGroup "WITH"
    [ selectUnionInsert genTestDatabase
    , rowsAffectedNoReturning genTestDatabase
    , rowsAffectedReturing genTestDatabase
    , pureQuery genTestDatabase
    ]
  where
    selectUnionInsert = 
      databasePropertyTest "Can UNION results of SELECT with results of INSERT" \transaction -> do
        rows <- forAll $ Gen.list (Range.linear 0 50) genTestTable

        transaction do
          rows' <- lift do
            statement () $ Rel8.run $ do
              values <- Rel8.select $ Rel8.values $ map Rel8.lit rows

              inserted <- Rel8.insert $ Rel8.Insert
                { into = testTableSchema
                , rows = values
                , onConflict = Rel8.DoNothing Nothing
                , returning = Rel8.Returning id
                }

              pure $ values <> inserted

          sort rows' === sort (rows <> rows)

    rowsAffectedNoReturning = 
      databasePropertyTest "Can read rows affected from INSERT without RETURNING" \transaction -> do
        rows <- forAll $ Gen.list (Range.linear 0 50) genTestTable

        transaction do
          affected <- lift do
            statement () $ Rel8.runN $ do
              Rel8.insert $ Rel8.Insert
                { into = testTableSchema
                , rows = Rel8.values $ map Rel8.lit rows
                , onConflict = Rel8.DoNothing Nothing
                , returning = Rel8.NoReturning
                }

          length rows === fromIntegral affected

    rowsAffectedReturing = 
      databasePropertyTest "Can read rows affected from INSERT with RETURNING" \transaction -> do
        rows <- forAll $ Gen.list (Range.linear 0 50) genTestTable

        transaction do
          affected <- lift do
            statement () $ Rel8.runN $ void $ do
              Rel8.insert $ Rel8.Insert
                { into = testTableSchema
                , rows = Rel8.values $ map Rel8.lit rows
                , onConflict = Rel8.DoNothing Nothing
                , returning = Rel8.Returning id
                }

          length rows === fromIntegral affected

    pureQuery = 
      databasePropertyTest "Can read pure Query" \transaction -> do
        rows <- forAll $ Gen.list (Range.linear 0 50) genTestTable

        transaction do
          rows' <- lift do
            statement () $ Rel8.run $ pure do
              Rel8.values $ map Rel8.lit rows

          sort rows === sort rows'



data UniqueTable f = UniqueTable
  { uniqueTableKey :: Rel8.Column f Text
  , uniqueTableValue :: Rel8.Column f Text
  }
  deriving stock Generic
  deriving anyclass Rel8.Rel8able


deriving stock instance Eq (UniqueTable Result)
deriving stock instance Ord (UniqueTable Result)
deriving stock instance Show (UniqueTable Result)


uniqueTableSchema :: Rel8.TableSchema (UniqueTable Rel8.Name)
uniqueTableSchema =
  Rel8.TableSchema
    { name = "unique_table"
    , columns = UniqueTable
        { uniqueTableKey = "key"
        , uniqueTableValue = "value"
        }
    }


genUniqueTable :: Gen (UniqueTable Result)
genUniqueTable = do
  uniqueTableKey <- Gen.text (Range.linear 0 5) Gen.alphaNum
  uniqueTableValue <- Gen.text (Range.linear 0 5) Gen.alphaNum
  pure UniqueTable {..}


testUpsert :: IO TmpPostgres.DB -> TestTree
testUpsert = databasePropertyTest "Can UPSERT UniqueTable" \transaction -> do
  as <- unique $ forAll $ Gen.list (Range.linear 0 20) genUniqueTable
  bs <- unique $ forAll $ Gen.list (Range.linear 0 20) genUniqueTable

  transaction do
    selected <- lift do
      statement () $ Rel8.run_ $ Rel8.insert Rel8.Insert
        { into = uniqueTableSchema
        , rows = Rel8.values $ Rel8.lit <$> as
        , onConflict = Rel8.DoNothing Nothing
        , returning = Rel8.NoReturning
        }

      statement () $ Rel8.run_ $ Rel8.insert Rel8.Insert
        { into = uniqueTableSchema
        , rows = Rel8.values $ Rel8.lit <$> bs
        , onConflict = Rel8.DoUpdate Rel8.Upsert
            { conflict =
                Rel8.OnIndex
                  Rel8.Index
                    { columns = uniqueTableKey
                    , predicate = Nothing
                    }
            , set = \UniqueTable {uniqueTableValue} old -> old {uniqueTableValue}
            , updateWhere = \_ _ -> Rel8.true
            }
        , returning = Rel8.NoReturning
        }

      statement () $ Rel8.run $ Rel8.select do
        Rel8.each uniqueTableSchema

    fromUniqueTables selected === fromUniqueTables bs <> fromUniqueTables as
  where
    unique = fmap (nubOrdOn uniqueTableKey)
    fromUniqueTables = Map.fromList . map \(UniqueTable key value) -> (key, value)


newtype HKNestedPair f = HKNestedPair { pairOne :: (TestTable f, TestTable f) }
  deriving stock Generic
  deriving anyclass Rel8.Rel8able

deriving stock instance Eq (HKNestedPair Result)
deriving stock instance Ord (HKNestedPair Result)
deriving stock instance Show (HKNestedPair Result)


testSelectNestedPairs :: IO TmpPostgres.DB -> TestTree
testSelectNestedPairs = databasePropertyTest "Can SELECT nested pairs" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) $ HKNestedPair <$> liftA2 (,) genTestTable genTestTable

  transaction do
    selected <- lift do
      statement () $ Rel8.run $ Rel8.select do
        Rel8.values $ map Rel8.lit rows

    sort selected === sort rows


testSelectArray :: IO TmpPostgres.DB -> TestTree
testSelectArray = databasePropertyTest "Can SELECT Arrays (with aggregation)" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 1 10) Gen.bool

  transaction do
    selected <- lift do
      statement () $ Rel8.run1 $ Rel8.select do
        Rel8.many $ Rel8.values (map Rel8.lit rows)

    selected === rows

    selected' <- lift do
      statement () $ Rel8.run $ Rel8.select do
        a <- Rel8.catListTable =<< do
          Rel8.many $ Rel8.values (map Rel8.lit rows)
        b <- Rel8.catListTable =<< do
          Rel8.many $ Rel8.values (map Rel8.lit rows)
        pure (a, b)

    selected' === liftA2 (,) rows rows


data NestedMaybeTable f = NestedMaybeTable
  { nmt1 :: Rel8.Column f Bool
  , nmt2 :: Rel8.HMaybe f (TestTable f)
  }
  deriving stock Generic
  deriving anyclass Rel8.Rel8able


deriving stock instance Eq (NestedMaybeTable Result)
deriving stock instance Ord (NestedMaybeTable Result)
deriving stock instance Show (NestedMaybeTable Result)


testNestedMaybeTable :: IO TmpPostgres.DB -> TestTree
testNestedMaybeTable = databasePropertyTest "Can nest MaybeTable within other tables" \transaction -> do
  let example = NestedMaybeTable { nmt1 = True, nmt2 = Just (TestTable "Hi" True) }

  transaction do
    selected <- lift do
      statement () $ Rel8.run1 $ Rel8.select do
        x <- Rel8.values [Rel8.lit example]
        pure $ Rel8.maybeTable (Rel8.lit False) (\_ -> Rel8.lit True) (nmt2 x)

    selected === True


testEvaluate :: IO TmpPostgres.DB -> TestTree
testEvaluate = databasePropertyTest "evaluate has the evaluation order we expect" \transaction -> do

  transaction do
    selected <- lift do
      statement () $ Rel8.run $ Rel8.select do
        x <- Rel8.values (Rel8.lit <$> ['a', 'b', 'c'])
        y <- Rel8.evaluate (Rel8.nextval "test_seq")
        pure (x, (y, y))

    normalize selected ===
      [ ('a', (0, 0))
      , ('b', (1, 1))
      , ('c', (2, 2))
      ]

    selected' <- lift do
      statement () $ Rel8.run $ Rel8.select do
        x <- Rel8.values (Rel8.lit <$> ['a', 'b', 'c'])
        y <- Rel8.values (Rel8.lit <$> ['d', 'e', 'f'])
        z <- Rel8.evaluate (Rel8.nextval "test_seq")
        pure ((x, y), (z, z))

    normalize selected' ===
      [ (('a', 'd'), (0, 0))
      , (('a', 'e'), (1, 1))
      , (('a', 'f'), (2, 2))
      , (('b', 'd'), (3, 3))
      , (('b', 'e'), (4, 4))
      , (('b', 'f'), (5, 5))
      , (('c', 'd'), (6, 6))
      , (('c', 'e'), (7, 7))
      , (('c', 'f'), (8, 8))
      ]

  where
    normalize :: [(x, (Int64, Int64))] -> [(x, (Int64, Int64))]
    normalize [] = []
    normalize xs@((_, (i, _)) : _) = map (fmap (\(a, b) -> (a - i, b - i))) xs
