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

module Main where

import Control.Exception ( bracket, throwIO )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Trans.Control ( MonadBaseControl, liftBaseOp_ )
import Data.Foldable ( for_ )
import Data.List ( nub, sort )
import Database.PostgreSQL.Simple ( Connection, connectPostgreSQL, close, withTransaction, execute_, executeMany, rollback )
import Database.PostgreSQL.Simple.SqlQQ ( sql )
import qualified Database.Postgres.Temp as TmpPostgres
import GHC.Generics ( Generic )
import Hedgehog ( Property, property, (===), forAll, cover, diff, evalM, PropertyT )
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
    , testLimit getTestDatabase
    , testUnion getTestDatabase
    , testDistinct getTestDatabase
    , testExists getTestDatabase
    , testOptional getTestDatabase
    ]

  where

    startTestDatabase = do
      db <- TmpPostgres.start >>= either throwIO return

      bracket (connectPostgreSQL (TmpPostgres.toConnectionString db)) close \conn -> do
        execute_ conn [sql|
          CREATE TABLE test_table ( column1 text not null, column2 bool not null );
        |]

      return (db)

    stopTestDatabase = TmpPostgres.stop

databasePropertyTest :: TestName -> (Connection -> PropertyT IO ()) -> IO TmpPostgres.DB -> TestTree
databasePropertyTest testName f getTestDatabase =
  withResource connect close $ \c ->
  testProperty testName $ property do
    connection <- liftIO c
    rollingBack connection (f connection)

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
testSelectTestTable = databasePropertyTest "Can SELECT TestTable" \connection -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  selected <- rollingBack connection do
    liftIO do
      executeMany connection
        [sql| INSERT INTO test_table (column1, column2) VALUES (?, ?) |]
        [ ( testTableColumn1, testTableColumn2 ) | TestTable{..} <- rows ]

    Rel8.select connection do
      Rel8.each testTableSchema

  sort selected === sort rows

  cover 1 "Empty" $ null rows
  cover 1 "Singleton" $ null $ drop 1 rows
  cover 1 ">1 row" $ not $ null $ drop 1 rows


testWhere_ :: IO TmpPostgres.DB -> TestTree
testWhere_ = databasePropertyTest "WHERE (Rel8.where_)" \connection -> do
  rows <- forAll $ Gen.list (Range.linear 1 10) genTestTable

  magicBool <- forAll Gen.bool

  let expected = filter (\t -> testTableColumn2 t == magicBool) rows

  selected <- rollingBack connection do
    Rel8.select connection do
      t <- Rel8.values $ Rel8.litTable <$> rows
      Rel8.where_ $ testTableColumn2 t Rel8.==. Rel8.lit magicBool
      return t

  sort selected === sort expected

  cover 1 "No results" $ null expected
  cover 1 "Some results" $ not $ null expected
  cover 1 "All results" $ expected == rows


testLimit :: IO TmpPostgres.DB -> TestTree
testLimit = databasePropertyTest "LIMIT (Rel8.limit)" \connection -> do
  rows <- forAll $ Gen.list (Range.linear 1 10) genTestTable

  n <- forAll $ Gen.integral (Range.linear 0 10)

  selected <- rollingBack connection do
    Rel8.select connection do
      Rel8.limit n $ Rel8.values (Rel8.litTable <$> rows)

  diff (length selected) (<=) (fromIntegral n)

  for_ selected \row ->
    diff row elem rows

  cover 1 "n == 0" $ n == 0
  cover 1 "n < length rows" $ fromIntegral n < length rows
  cover 1 "n == length rows" $ fromIntegral n == length rows
  cover 1 "n >= length rows" $ fromIntegral n >= length rows


testUnion :: IO TmpPostgres.DB -> TestTree
testUnion = databasePropertyTest "UNION (Rel8.union)" \connection -> do
  left <- forAll $ Gen.list (Range.linear 0 10) genTestTable
  right <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  selected <- evalM $ rollingBack connection do
    Rel8.select connection do
      Rel8.values (Rel8.litTable <$> left) `Rel8.union` Rel8.values (Rel8.litTable <$> right)

  sort selected === sort (left ++ right)


testDistinct :: IO TmpPostgres.DB -> TestTree
testDistinct = databasePropertyTest "DISTINCT (Rel8.distinct)" \connection -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  selected <- evalM $ rollingBack connection do
    Rel8.select connection
      $ Rel8.distinct
      $ Rel8.values (Rel8.litTable <$> rows)

  sort selected === nub (sort rows)

  cover 1 "Empty" $ null rows
  cover 1 "Duplicates" $ not (null rows) && rows /= nub rows
  cover 1 "No duplicates" $ not (null rows) && rows == nub rows


testExists :: IO TmpPostgres.DB -> TestTree
testExists = databasePropertyTest "EXISTS (Rel8.exists)" \connection -> do
  rows1 <- forAll $ Gen.list (Range.linear 1 10) genTestTable
  rows2 <- forAll $ Gen.maybe genTestTable

  selected <- evalM $ rollingBack connection do
    Rel8.select connection do
      row <- Rel8.values $ Rel8.litTable <$> rows1
      Rel8.exists do
        Rel8.values $ Rel8.litTable <$> rows2
      return row

  case rows2 of
    Nothing -> sort selected === []
    Just{}  -> sort selected === sort rows1

  cover 1 "EXISTS = False" $ null rows2
  cover 1 "EXISTS = True" $ not $ null rows2


testOptional :: IO TmpPostgres.DB -> TestTree
testOptional = databasePropertyTest "Rel8.optional" \connection -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  selected <- evalM $ rollingBack connection do
    Rel8.select connection $
      Rel8.optional $ Rel8.values $ Rel8.litTable <$> rows

  case rows of
    [] -> selected === [Nothing]
    _  -> sort selected === fmap Just (sort rows)


rollingBack
  :: (MonadBaseControl IO m, MonadIO m)
  => Connection -> m a -> m a
rollingBack connection m =
  liftBaseOp_ (withTransaction connection) $ m <* liftIO (rollback connection)


genTestTable = do
  testTableColumn1 <- Gen.list (Range.linear 0 5) Gen.alphaNum
  testTableColumn2 <- Gen.bool
  return TestTable{..}
