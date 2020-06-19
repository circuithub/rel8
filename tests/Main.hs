{-# language NamedFieldPuns #-}
{-# language BlockArguments #-}
{-# language QuasiQuotes #-}
{-# language DisambiguateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language DeriveGeneric #-}
{-# language StandaloneDeriving #-}
{-# language DeriveAnyClass #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language TypeApplications #-}

module Main where

import Data.Functor ( (<&>) )
import Control.Applicative ( liftA2, liftA3 )
import Control.Exception.Lifted ( bracket, throwIO, finally )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Trans.Control ( MonadBaseControl, liftBaseOp_ )
import Data.Bifunctor ( bimap )
import qualified Data.ByteString.Lazy
import Data.CaseInsensitive (mk)
import Data.Foldable ( for_ )
import Data.Function ( on )
import Data.Functor ( void )
import Data.Int ( Int32, Int64 )
import Data.List ( nub, sort )
import qualified Data.Map.Strict as Map
import Data.Maybe ( catMaybes )
import Data.Scientific ( Scientific )
import Data.String ( fromString )
import qualified Data.Text.Lazy
import Data.Time
import qualified Data.UUID
import Database.PostgreSQL.Simple ( Connection, connectPostgreSQL, close, withTransaction, execute_, executeMany, rollback )
import Database.PostgreSQL.Simple.SqlQQ ( sql )
import qualified Database.Postgres.Temp as TmpPostgres
import GHC.Generics ( Generic )
import Hedgehog ( property, (===), forAll, cover, diff, evalM, PropertyT, TestT, test, Gen )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Rel8
import Test.Tasty
import Test.Tasty.Hedgehog ( testProperty )


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests =
  withResource startTestDatabase stopTestDatabase \getTestDatabase ->
  testGroup "rel8"
    [ testSelectTestTable getTestDatabase
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
    , testIfThenElse getTestDatabase
    , testAp getTestDatabase
    , testDBType getTestDatabase
    , testDBEq getTestDatabase
    -- , TODO testTableEquality getTestDatabase
    , testFromString getTestDatabase
    , testCatMaybeTable getTestDatabase
    , testCatMaybe getTestDatabase
    , testMaybeTable getTestDatabase
    , testNestedTables getTestDatabase
    , testMaybeTableApplicative getTestDatabase
    , testLogicalFixities getTestDatabase
    , testUpdate getTestDatabase
    , testDelete getTestDatabase
    ]

  where

    startTestDatabase = do
      db <- TmpPostgres.start >>= either throwIO return

      bracket (connectPostgreSQL (TmpPostgres.toConnectionString db)) close \conn -> do
        void $ execute_ conn [sql|
          CREATE EXTENSION citext;
          CREATE TABLE test_table ( column1 text not null, column2 bool not null );
        |]

      return db

    stopTestDatabase = TmpPostgres.stop

databasePropertyTest
  :: TestName
  -> (((Connection -> TestT IO ()) -> PropertyT IO ()) -> PropertyT IO ())
  -> IO TmpPostgres.DB -> TestTree
databasePropertyTest testName f getTestDatabase =
  withResource connect close $ \c ->
  testProperty testName $ property do
    connection <- liftIO c
    f \g -> test $ rollingBack connection $ g connection

  where

    connect = connectPostgreSQL . TmpPostgres.toConnectionString =<< getTestDatabase


data TestTable f = TestTable
  { testTableColumn1 :: Rel8.Column f String
  , testTableColumn2 :: Rel8.Column f Bool
  }
  deriving
    ( Generic, Rel8.HigherKindedTable )


deriving instance Eq (TestTable Rel8.Identity)
deriving instance Ord (TestTable Rel8.Identity)
deriving instance Show (TestTable Rel8.Identity)


testTableSchema :: Rel8.TableSchema ( TestTable Rel8.ColumnSchema )
testTableSchema =
  Rel8.TableSchema
    { tableName = "test_table"
    , tableSchema = Nothing
    , tableColumns = TestTable
        { testTableColumn1 = "column1"
        , testTableColumn2 = "column2"
        }
    }


testSelectTestTable :: IO TmpPostgres.DB -> TestTree
testSelectTestTable = databasePropertyTest "Can SELECT TestTable" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  transaction \connection -> do
    _ <- Rel8.insert connection
      Rel8.Insert
        { into = testTableSchema
        , rows = map Rel8.litTable rows
        , onConflict = Rel8.DoNothing
        , returning = Rel8.NumberOfRowsInserted
        }

    selected <- Rel8.select connection do
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

  transaction \connection -> do
    selected <- Rel8.select connection do
      t <- Rel8.values $ Rel8.litTable <$> rows
      Rel8.where_ $ testTableColumn2 t Rel8.==. Rel8.lit magicBool
      return t

    sort selected === sort expected

    cover 1 "No results" $ null expected
    cover 1 "Some results" $ not $ null expected
    cover 1 "All results" $ expected == rows


testFilter :: IO TmpPostgres.DB -> TestTree
testFilter = databasePropertyTest "filter" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 1 10) genTestTable

  transaction \connection -> do
    let expected = filter testTableColumn2 rows

    selected <- Rel8.select connection
      $ Rel8.filter testTableColumn2 =<< Rel8.values (Rel8.litTable <$> rows)

    sort selected === sort expected

    cover 1 "No results" $ null expected
    cover 1 "Some results" $ not $ null expected
    cover 1 "All results" $ expected == rows


testLimit :: IO TmpPostgres.DB -> TestTree
testLimit = databasePropertyTest "LIMIT (Rel8.limit)" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 1 10) genTestTable

  n <- forAll $ Gen.integral (Range.linear 0 10)

  transaction \connection -> do
    selected <- Rel8.select connection do
      Rel8.limit n $ Rel8.values (Rel8.litTable <$> rows)

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

  transaction \connection -> do
    selected <- Rel8.select connection do
      Rel8.values (Rel8.litTable <$> nub left) `Rel8.union` Rel8.values (Rel8.litTable <$> nub right)

    sort selected === sort (nub (left ++ right))


testDistinct :: IO TmpPostgres.DB -> TestTree
testDistinct = databasePropertyTest "DISTINCT (Rel8.distinct)" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  transaction \connection -> do
    selected <- Rel8.select connection $ Rel8.distinct do
      Rel8.values (Rel8.litTable <$> rows)

    sort selected === nub (sort rows)

    cover 1 "Empty" $ null rows
    cover 1 "Duplicates" $ not (null rows) && rows /= nub rows
    cover 1 "No duplicates" $ not (null rows) && rows == nub rows


testExists :: IO TmpPostgres.DB -> TestTree
testExists = databasePropertyTest "EXISTS (Rel8.exists)" \transaction -> do
  rows1 <- forAll $ Gen.list (Range.linear 1 10) genTestTable
  rows2 <- forAll $ Gen.maybe genTestTable

  transaction \connection -> do
    selected <- Rel8.select connection do
      row <- Rel8.values $ Rel8.litTable <$> rows1
      _ <- Rel8.exists do
        Rel8.values $ Rel8.litTable <$> rows2
      return row

    case rows2 of
      Nothing -> sort selected === []
      Just{}  -> sort selected === sort rows1

    cover 1 "EXISTS = False" $ null rows2
    cover 1 "EXISTS = True" $ not $ null rows2


testOptional :: IO TmpPostgres.DB -> TestTree
testOptional = databasePropertyTest "Rel8.optional" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  transaction \connection -> do
    _ <- liftIO do
      executeMany connection
        [sql| INSERT INTO test_table (column1, column2) VALUES (?, ?) |]
        [ ( testTableColumn1, testTableColumn2 ) | TestTable{..} <- rows ]

    selected <- Rel8.select connection do
      Rel8.optional $ Rel8.each testTableSchema

    case rows of
      [] -> selected === [Nothing]
      _  -> sort selected === fmap Just (sort rows)


testAnd :: IO TmpPostgres.DB -> TestTree
testAnd = databasePropertyTest "AND (&&.)" \transaction -> do
  (x, y) <- forAll $ liftA2 (,) Gen.bool Gen.bool

  transaction \connection -> do
    [result] <- Rel8.select connection $ pure $
      Rel8.lit x Rel8.&&. Rel8.lit y

    result === (x && y)


testOr :: IO TmpPostgres.DB -> TestTree
testOr = databasePropertyTest "OR (||.)" \transaction -> do
  (x, y) <- forAll $ liftA2 (,) Gen.bool Gen.bool

  transaction \connection -> do
    [result] <- Rel8.select connection $ pure $
      Rel8.lit x Rel8.||. Rel8.lit y

    result === (x || y)


testLogicalFixities :: IO TmpPostgres.DB -> TestTree
testLogicalFixities = databasePropertyTest "Logical operator fixities" \transaction -> do
  (u, v, w, x) <- forAll $ (,,,) <$> Gen.bool <*> Gen.bool <*> Gen.bool <*> Gen.bool

  transaction \connection -> do
    [result] <- Rel8.select connection $ pure $
      Rel8.lit u Rel8.||. Rel8.lit v Rel8.&&. Rel8.lit w Rel8.==. Rel8.lit x

    result === (u || v && w == x)


testNot :: IO TmpPostgres.DB -> TestTree
testNot = databasePropertyTest "NOT (not_)" \transaction -> do
  x <- forAll Gen.bool

  transaction \connection -> do
    [result] <- Rel8.select connection $ pure $
      Rel8.not_ $ Rel8.lit x

    result === not x


testIfThenElse :: IO TmpPostgres.DB -> TestTree
testIfThenElse = databasePropertyTest "ifThenElse_" \transaction -> do
  (x, y, z) <- forAll $ liftA3 (,,) Gen.bool Gen.bool Gen.bool

  transaction \connection -> do
    [result] <- Rel8.select connection $ pure $
      Rel8.ifThenElse_ (Rel8.lit x) (Rel8.lit y) (Rel8.lit z)

    result === if x then y else z


testAp :: IO TmpPostgres.DB -> TestTree
testAp = databasePropertyTest "Cartesian product (<*>)" \transaction -> do
  (rows1, rows2) <- forAll $
    liftA2 (,)
      (Gen.list (Range.linear 1 10) genTestTable)
      (Gen.list (Range.linear 1 10) genTestTable)

  transaction \connection -> do
    result <- Rel8.select connection $ do
      liftA2 (,) (Rel8.values (Rel8.litTable <$> rows1)) (Rel8.values (Rel8.litTable <$> rows2))

    sort result === sort (liftA2 (,) rows1 rows2)


testDBType :: IO TmpPostgres.DB -> TestTree
testDBType getTestDatabase = testGroup "DBType instances"
  [ dbTypeTest "Bool" Gen.bool
  , dbTypeTest "ByteString" $ Gen.bytes (Range.linear 0 128)
  , dbTypeTest "CI Lazy Text" $ mk . Data.Text.Lazy.fromStrict <$> Gen.text (Range.linear 0 10) Gen.unicode
  , dbTypeTest "CI Text" $ mk <$> Gen.text (Range.linear 0 10) Gen.unicode
  , dbTypeTest "Day" genDay
  , dbTypeTest "Double" $ (/10) . fromInteger @Double <$> Gen.integral (Range.linear (-100) 100)
  , dbTypeTest "Float" $ (/10) . fromInteger @Float <$> Gen.integral (Range.linear (-100) 100)
  , dbTypeTest "Int32" $ Gen.integral @_ @Int32 Range.linearBounded
  , dbTypeTest "Int64" $ Gen.integral @_ @Int64 Range.linearBounded
  , dbTypeTest "Lazy ByteString" $ Data.ByteString.Lazy.fromStrict <$> Gen.bytes (Range.linear 0 128)
  , dbTypeTest "Lazy Text" $ Data.Text.Lazy.fromStrict <$> Gen.text (Range.linear 0 10) Gen.unicode
  , dbTypeTest "LocalTime" genLocalTime
  , dbTypeTest "Scientific" $ (/10) . fromInteger @Scientific <$> Gen.integral (Range.linear (-100) 100)
  , dbTypeTest "String" $ Gen.list (Range.linear 0 10) Gen.unicode
  , dbTypeTest "Text" $ Gen.text (Range.linear 0 10) Gen.unicode
  , dbTypeTest "TimeOfDay" genTimeOfDay
  , dbTypeTest "UTCTime" $ UTCTime <$> genDay <*> genDiffTime
  , dbTypeTest "UUID" $ Data.UUID.fromWords <$> genWord32 <*> genWord32 <*> genWord32 <*> genWord32
  , dbTypeTestEq ((==) `on` zonedTimeToUTC) "ZonedTime" $ ZonedTime <$> genLocalTime <*> genTimeZone
  ]

  where
    dbTypeTest name generator = testGroup name
      [ databasePropertyTest name (t (==) generator) getTestDatabase
      , databasePropertyTest ("Maybe " <> name) (t (==) (Gen.maybe generator)) getTestDatabase
      ]

    dbTypeTestEq f name generator = testGroup name
      [ databasePropertyTest name (t f generator) getTestDatabase
      , databasePropertyTest ("Maybe " <> name) (t (maybeEq f) (Gen.maybe generator)) getTestDatabase
      ]

    maybeEq _ Nothing Nothing = True
    maybeEq _ Just{} Nothing = False
    maybeEq _ Nothing Just{} = False
    maybeEq f (Just x) (Just y) = f x y

    t eq generator transaction = do
      x <- forAll generator

      transaction \connection -> do
        [res] <- Rel8.select connection $ pure $ Rel8.lit x
        diff res eq x

    genDay = do
      year <- Gen.integral (Range.linear 1970 3000)
      month <- Gen.integral (Range.linear 1 12)
      day <- Gen.integral (Range.linear 1 31)
      Gen.just $ pure $ fromGregorianValid year month day

    genDiffTime = secondsToDiffTime <$> Gen.integral (Range.linear 0 86401)

    genTimeOfDay = do
      hour <- Gen.integral (Range.linear 0 23)
      minutes <- Gen.integral (Range.linear 0 59)
      sec <- fromInteger <$> Gen.integral (Range.linear 0 59)
      Gen.just $ pure $ makeTimeOfDayValid hour minutes sec

    genLocalTime = LocalTime <$> genDay <*> genTimeOfDay

    genTimeZone = hoursToTimeZone <$> Gen.integral (Range.linear (-6) 6)

    genWord32 = Gen.integral Range.linearBounded


testDBEq :: IO TmpPostgres.DB -> TestTree
testDBEq getTestDatabase = testGroup "DBEq instances"
  [ dbEqTest "Bool" Gen.bool
  , dbEqTest "Int32" $ Gen.integral @_ @Int32 Range.linearBounded
  , dbEqTest "Int64" $ Gen.integral @_ @Int64 Range.linearBounded
  , dbEqTest "Text" $ Gen.text (Range.linear 0 10) Gen.unicode
  , dbEqTest "String" $ Gen.list (Range.linear 0 10) Gen.unicode
  ]

  where
    dbEqTest name generator = testGroup name
      [ databasePropertyTest name (t generator) getTestDatabase
      , databasePropertyTest ("Maybe " <> name) (t (Gen.maybe generator)) getTestDatabase
      ]
    t generator transaction = do
      (x, y) <- forAll do
        Gen.frequency
          [ (9, liftA2 (,) generator generator)
          , (1, generator <&> \x -> (x, x))
          ]

      transaction \connection -> do
        [res] <- Rel8.select connection $ pure $ Rel8.lit x Rel8.==. Rel8.lit y
        res === (x == y)

        cover 1 "Equal" $ x == y
        cover 1 "Not Equal" $ x /= y


-- testTableEquality :: IO TmpPostgres.DB -> TestTree
-- testTableEquality = databasePropertyTest "TestTable equality" \connection -> do
--   (x, y) <- forAll $ liftA2 (,) genTestTable genTestTable

--   [eq] <- Rel8.select connection do
--     pure $ Rel8.litTable x Rel8.==. Rel8.litTable y

--   eq === (x == y)

--   cover 1 "Equal" $ x == y
--   cover 1 "Not Equal" $ x /= y


testFromString :: IO TmpPostgres.DB -> TestTree
testFromString = databasePropertyTest "FromString" \transaction -> do
  str <- forAll $ Gen.list (Range.linear 0 10) Gen.unicode

  transaction \connection -> do
    [result] <- Rel8.select connection $ pure $ fromString str
    result === str


testCatMaybeTable :: IO TmpPostgres.DB -> TestTree
testCatMaybeTable = databasePropertyTest "catMaybeTable" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  transaction \connection -> do
    selected <- Rel8.select connection do
      testTable <- Rel8.values $ Rel8.litTable <$> rows
      Rel8.catMaybeTable $ Rel8.ifThenElse_ (testTableColumn2 testTable) (pure testTable) Rel8.noTable

    sort selected === sort (filter testTableColumn2 rows)


testCatMaybe :: IO TmpPostgres.DB -> TestTree
testCatMaybe = databasePropertyTest "catMaybe" \transaction -> evalM do
  rows <- forAll $ Gen.list (Range.linear 0 10) $ Gen.maybe Gen.bool

  transaction \connection -> do
    selected <- evalM $ Rel8.select connection do
      Rel8.catMaybe =<< Rel8.values (map Rel8.lit rows)

    sort selected === sort (catMaybes rows)


testMaybeTable :: IO TmpPostgres.DB -> TestTree
testMaybeTable = databasePropertyTest "maybeTable" \transaction -> evalM do
  (rows, def) <- forAll $ liftA2 (,) (Gen.list (Range.linear 0 10) genTestTable) genTestTable

  transaction \connection -> do
    _ <- liftIO $ executeMany connection
      [sql| INSERT INTO test_table (column1, column2) VALUES (?, ?) |]
      [ ( testTableColumn1, testTableColumn2 ) | TestTable{..} <- rows ]

    selected <- Rel8.select connection $
      Rel8.maybeTable (Rel8.litTable def) id <$> Rel8.optional (Rel8.each testTableSchema)

    case rows of
      [] -> selected === [def]
      _ -> sort selected === sort rows


data TwoTestTables f =
  TwoTestTables
    { testTable1 :: TestTable f
    , testTable2 :: TestTable f
    }
  deriving
    ( Generic, Rel8.HigherKindedTable )


deriving instance Eq (TwoTestTables Rel8.Identity)
deriving instance Ord (TwoTestTables Rel8.Identity)
deriving instance Show (TwoTestTables Rel8.Identity)


testNestedTables :: IO TmpPostgres.DB -> TestTree
testNestedTables = databasePropertyTest "Nested TestTables" \transaction -> evalM do
  rows <- forAll do
    Gen.list (Range.linear 0 10) $
      liftA2 TwoTestTables genTestTable genTestTable

  transaction \connection -> do
    selected <- Rel8.select connection do
      Rel8.values (Rel8.litTable <$> rows)

    sort selected === sort rows


testMaybeTableApplicative :: IO TmpPostgres.DB -> TestTree
testMaybeTableApplicative = databasePropertyTest "MaybeTable (<*>)" \transaction -> evalM do
  rows <- forAll do
    Gen.list (Range.linear 0 10) $ liftA2 TestTable (Gen.list (Range.linear 0 10) Gen.unicode) (pure True)

  transaction \connection -> do
    _ <- liftIO $ executeMany connection
      [sql| INSERT INTO test_table (column1, column2) VALUES (?, ?) |]
      [ ( testTableColumn1, testTableColumn2 ) | TestTable{..} <- rows ]

    selected <- Rel8.select connection do
      fmap (pure id <*>) (Rel8.optional (Rel8.each testTableSchema))

    let rowsExpected = case rows of
          [] -> [Nothing]
          xs -> map Just xs

    sort selected === sort rowsExpected


rollingBack
  :: (MonadBaseControl IO m, MonadIO m)
  => Connection -> m a -> m a
rollingBack connection m =
  liftBaseOp_ (withTransaction connection) do
    m `finally` liftIO (rollback connection)


genTestTable :: Gen ( TestTable Rel8.Identity )
genTestTable = do
  testTableColumn1 <- Gen.list (Range.linear 0 5) Gen.alphaNum
  testTableColumn2 <- Gen.bool
  return TestTable{..}


testUpdate :: IO TmpPostgres.DB -> TestTree
testUpdate = databasePropertyTest "Can UPDATE TestTable" \transaction -> do
  rows <- forAll $ Gen.map (Range.linear 0 5) $ liftA2 (,) genTestTable genTestTable

  transaction \connection -> do
    _ <- Rel8.insert connection
      Rel8.Insert
        { into = testTableSchema
        , rows = map Rel8.litTable $ Map.keys rows
        , onConflict = Rel8.DoNothing
        , returning = Rel8.NumberOfRowsInserted
        }

    n <- Rel8.update connection
      Rel8.Update
        { target = testTableSchema
        , set = \r ->
            let updates = map (bimap Rel8.litTable Rel8.litTable) $ Map.toList rows
            in
            foldl
              ( \e (x, y) ->
                  Rel8.ifThenElse_
                    ( testTableColumn1 r Rel8.==. testTableColumn1 x Rel8.&&.
                      testTableColumn2 r Rel8.==. testTableColumn2 x
                    )
                    y
                    e
              )
              r
              updates
        , updateWhere = \_ -> Rel8.lit True
        , returning = Rel8.NumberOfRowsInserted
        }

    n === fromIntegral (length rows)

    selected <- Rel8.select connection do
      Rel8.each testTableSchema

    sort selected === sort (Map.elems rows)

    cover 1 "Empty" $ null rows
    cover 1 "Singleton" $ null $ drop 1 $ Map.keys rows
    cover 1 ">1 row" $ not $ null $ drop 1 $ Map.keys rows


testDelete :: IO TmpPostgres.DB -> TestTree
testDelete = databasePropertyTest "Can DELETE TestTable" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 5) genTestTable

  transaction \connection -> do
    _ <- Rel8.insert connection
      Rel8.Insert
        { into = testTableSchema
        , rows = map Rel8.litTable rows
        , onConflict = Rel8.DoNothing
        , returning = Rel8.NumberOfRowsInserted
        }

    deleted <-
      Rel8.delete connection
        Rel8.Delete
          { from = testTableSchema
          , deleteWhere = testTableColumn2
          , returning = Rel8.Projection id
          }

    selected <- Rel8.select connection do
      Rel8.each testTableSchema

    sort (deleted <> selected) === sort rows
