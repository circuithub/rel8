{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Main
  ( main
  )
where

-- base
import Control.Applicative ( liftA2, liftA3 )
import Control.Monad (void)
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Bifunctor ( bimap )
import Data.Foldable ( for_ )
import Data.Functor.Identity ( Identity )
import Data.Int ( Int32, Int64 )
import Data.List ( nub, sort )
import Data.Maybe ( catMaybes )
import Data.String ( fromString )
import Data.Word (Word32)
import GHC.Generics ( Generic )

-- bytestring
import qualified Data.ByteString.Lazy

-- case-insensitive
import Data.CaseInsensitive (mk)

-- containers
import qualified Data.Map.Strict as Map

-- hasql
import Hasql.Connection ( Connection,  acquire, release )
import Hasql.Session ( sql, run )

-- hedgehog
import Hedgehog ( property, (===), forAll, cover, diff, evalM, PropertyT, TestT, test, Gen )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- lifted-base
import Control.Exception.Lifted ( bracket, throwIO, bracket_ )

-- monad-control
import Control.Monad.Trans.Control ( MonadBaseControl )

-- rel8
import qualified Rel8.Expr as Rel8
import qualified Rel8.Expr.Bool as Rel8
import qualified Rel8.Expr.Eq as Rel8
import qualified Rel8.Query.Aggregate as Rel8
import qualified Rel8.Query.Distinct as Rel8
import qualified Rel8.Query.Each as Rel8
import qualified Rel8.Query.Exists as Rel8
import qualified Rel8.Query.Filter as Rel8
import qualified Rel8.Query.Limit as Rel8
import qualified Rel8.Query.Maybe as Rel8
import qualified Rel8.Query.Null as Rel8
import qualified Rel8.Query.Set as Rel8
import qualified Rel8.Query.Values as Rel8
import qualified Rel8.Schema.Column as Rel8
import qualified Rel8.Schema.Context as Rel8
import qualified Rel8.Schema.Generic as Rel8
import qualified Rel8.Schema.Nullability as Rel8
import qualified Rel8.Schema.Table as Rel8
import qualified Rel8.Statement.Delete as Rel8
import qualified Rel8.Statement.Insert as Rel8
import qualified Rel8.Statement.Returning as Rel8
import qualified Rel8.Statement.Select as Rel8
import qualified Rel8.Statement.Update as Rel8
import qualified Rel8.Table.Aggregate as Rel8
import qualified Rel8.Table.Bool as Rel8
import qualified Rel8.Table.Eq as Rel8
import qualified Rel8.Table.Insert as Rel8
import qualified Rel8.Table.Maybe as Rel8
import qualified Rel8.Table.Serialize as Rel8
import qualified Rel8.Type as Rel8
import qualified Rel8.Type.Eq as Rel8

-- scientific
import Data.Scientific ( Scientific )

-- tasty
import Test.Tasty

-- tasty-hedgehog
import Test.Tasty.Hedgehog ( testProperty )

-- text
import Data.Text (Text, pack)
import qualified Data.Text.Lazy

-- time
import Data.Time

-- tmp-postgres
import qualified Database.Postgres.Temp as TmpPostgres

-- uuid
import qualified Data.UUID


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
    , testBool getTestDatabase
    , testAp getTestDatabase
    , testDBType getTestDatabase
    , testDBEq getTestDatabase
    , testTableEquality getTestDatabase
    , testFromString getTestDatabase
    , testCatMaybeTable getTestDatabase
    , testCatMaybe getTestDatabase
    , testMaybeTable getTestDatabase
    , testNestedTables getTestDatabase
    , testMaybeTableApplicative getTestDatabase
    , testLogicalFixities getTestDatabase
    , testUpdate getTestDatabase
    , testDelete getTestDatabase
    , testSelectNestedPairs getTestDatabase
    , testSelectArray getTestDatabase
    , testNestedMaybeTable getTestDatabase
    ]

  where

    startTestDatabase = do
      db <- TmpPostgres.start >>= either throwIO return

      bracket (either (error . show) return =<< acquire (TmpPostgres.toConnectionString db)) release \conn -> void do
        flip run conn do
          sql "CREATE EXTENSION citext"
          sql "CREATE TABLE test_table ( column1 text not null, column2 bool not null )"

      return db

    stopTestDatabase = TmpPostgres.stop


databasePropertyTest
  :: TestName
  -> (((Connection -> TestT IO ()) -> PropertyT IO ()) -> PropertyT IO ())
  -> IO TmpPostgres.DB -> TestTree
databasePropertyTest testName f getTestDatabase =
  withResource connect release $ \c ->
  testProperty testName $ property do
    connection <- liftIO c
    f \g -> test $ rollingBack connection $ g connection

  where

    connect = either (error . show) return =<< acquire . TmpPostgres.toConnectionString =<< getTestDatabase


data TestTable f = TestTable
  { testTableColumn1 :: Rel8.Column f Text
  , testTableColumn2 :: Rel8.Column f Bool
  }
  deriving stock Generic
  deriving anyclass Rel8.Rel8able


deriving stock instance Eq (TestTable Identity)
deriving stock instance Ord (TestTable Identity)
deriving stock instance Show (TestTable Identity)


testTableSchema :: Rel8.TableSchema (TestTable Rel8.Name)
testTableSchema =
  Rel8.TableSchema
    { name = "test_table"
    , schema = Nothing
    , columns = TestTable
        { testTableColumn1 = "column1"
        , testTableColumn2 = "column2"
        }
    }


testSelectTestTable :: IO TmpPostgres.DB -> TestTree
testSelectTestTable = databasePropertyTest "Can SELECT TestTable" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  transaction \connection -> do
    void do
      liftIO $ Rel8.insert connection
        Rel8.Insert
          { into = testTableSchema
          , rows = map (Rel8.toInsert . Rel8.lit) rows
          , onConflict = Rel8.DoNothing
          , returning = Rel8.NumberOfRowsAffected
          }

    selected <- liftIO $ Rel8.select connection do
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
    selected <- liftIO $ Rel8.select connection do
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

  transaction \connection -> do
    let expected = filter testTableColumn2 rows

    selected <- liftIO $ Rel8.select connection
      $ Rel8.filter testTableColumn2 =<< Rel8.values (Rel8.lit <$> rows)

    sort selected === sort expected

    cover 1 "No results" $ null expected
    cover 1 "Some results" $ not $ null expected
    cover 1 "All results" $ expected == rows


testLimit :: IO TmpPostgres.DB -> TestTree
testLimit = databasePropertyTest "LIMIT (Rel8.limit)" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 1 10) genTestTable

  n <- forAll $ Gen.integral (Range.linear 0 10)

  transaction \connection -> do
    selected <- liftIO $ Rel8.select connection do
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

  transaction \connection -> do
    selected <- liftIO $ Rel8.select connection do
      Rel8.values (Rel8.lit <$> nub left) `Rel8.union` Rel8.values (Rel8.lit <$> nub right)

    sort selected === sort (nub (left ++ right))


testDistinct :: IO TmpPostgres.DB -> TestTree
testDistinct = databasePropertyTest "DISTINCT (Rel8.distinct)" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  transaction \connection -> do
    selected <- liftIO $ Rel8.select connection $ Rel8.distinct do
      Rel8.values (Rel8.lit <$> rows)

    sort selected === nub (sort rows)

    cover 1 "Empty" $ null rows
    cover 1 "Duplicates" $ not (null rows) && rows /= nub rows
    cover 1 "No duplicates" $ not (null rows) && rows == nub rows


testExists :: IO TmpPostgres.DB -> TestTree
testExists = databasePropertyTest "EXISTS (Rel8.exists)" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  transaction \connection -> do
    exists <- liftIO $ Rel8.select connection $ Rel8.exists $ Rel8.values $ Rel8.lit <$> rows

    case rows of
      [] -> exists === [False]
      _ -> exists === [True]


testOptional :: IO TmpPostgres.DB -> TestTree
testOptional = databasePropertyTest "Rel8.optional" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  transaction \connection -> do
    selected <- liftIO $ Rel8.select connection do
      Rel8.optional $ Rel8.values (Rel8.lit <$> rows)

    case rows of
      [] -> selected === [Nothing]
      _  -> sort selected === fmap Just (sort rows)


testAnd :: IO TmpPostgres.DB -> TestTree
testAnd = databasePropertyTest "AND (&&.)" \transaction -> do
  (x, y) <- forAll $ liftA2 (,) Gen.bool Gen.bool

  transaction \connection -> do
    [result] <- liftIO $ Rel8.select connection $ pure $
      Rel8.lit x Rel8.&&. Rel8.lit y

    result === (x && y)


testOr :: IO TmpPostgres.DB -> TestTree
testOr = databasePropertyTest "OR (||.)" \transaction -> do
  (x, y) <- forAll $ liftA2 (,) Gen.bool Gen.bool

  transaction \connection -> do
    [result] <- liftIO $ Rel8.select connection $ pure $
      Rel8.lit x Rel8.||. Rel8.lit y

    result === (x || y)


testLogicalFixities :: IO TmpPostgres.DB -> TestTree
testLogicalFixities = databasePropertyTest "Logical operator fixities" \transaction -> do
  (u, v, w, x) <- forAll $ (,,,) <$> Gen.bool <*> Gen.bool <*> Gen.bool <*> Gen.bool

  transaction \connection -> do
    [result] <- liftIO $ Rel8.select connection $ pure $
      Rel8.lit u Rel8.||. Rel8.lit v Rel8.&&. Rel8.lit w Rel8.==. Rel8.lit x

    result === (u || v && w == x)


testNot :: IO TmpPostgres.DB -> TestTree
testNot = databasePropertyTest "NOT (not_)" \transaction -> do
  x <- forAll Gen.bool

  transaction \connection -> do
    [result] <- liftIO $ Rel8.select connection $ pure $
      Rel8.not_ $ Rel8.lit x

    result === not x


testBool :: IO TmpPostgres.DB -> TestTree
testBool = databasePropertyTest "ifThenElse_" \transaction -> do
  (x, y, z) <- forAll $ liftA3 (,,) Gen.bool Gen.bool Gen.bool

  transaction \connection -> do
    [result] <- liftIO $ Rel8.select connection $ pure $
      Rel8.bool (Rel8.lit z) (Rel8.lit y) (Rel8.lit x)

    result === if x then y else z


testAp :: IO TmpPostgres.DB -> TestTree
testAp = databasePropertyTest "Cartesian product (<*>)" \transaction -> do
  (rows1, rows2) <- forAll $
    liftA2 (,)
      (Gen.list (Range.linear 1 10) genTestTable)
      (Gen.list (Range.linear 1 10) genTestTable)

  transaction \connection -> do
    result <- liftIO $ Rel8.select connection $ do
      liftA2 (,) (Rel8.values (Rel8.lit <$> rows1)) (Rel8.values (Rel8.lit <$> rows2))

    sort result === sort (liftA2 (,) rows1 rows2)


testDBType :: IO TmpPostgres.DB -> TestTree
testDBType getTestDatabase = testGroup "DBType instances"
  [ dbTypeTest "Bool" Gen.bool
  , dbTypeTest "ByteString" $ Gen.bytes (Range.linear 0 128)
  , dbTypeTest "CI Lazy Text" $ mk . Data.Text.Lazy.fromStrict <$> Gen.text (Range.linear 0 10) Gen.unicode
  , dbTypeTest "CI Text" $ mk <$> Gen.text (Range.linear 0 10) Gen.unicode
  , dbTypeTest "Day" genDay
  , dbTypeTest "Double" $ (/10) . fromIntegral @Int @Double <$> Gen.integral (Range.linear (-100) 100)
  , dbTypeTest "Float" $ (/10) . fromIntegral @Int @Float <$> Gen.integral (Range.linear (-100) 100)
  , dbTypeTest "Int32" $ Gen.integral @_ @Int32 Range.linearBounded
  , dbTypeTest "Int64" $ Gen.integral @_ @Int64 Range.linearBounded
  , dbTypeTest "Lazy ByteString" $ Data.ByteString.Lazy.fromStrict <$> Gen.bytes (Range.linear 0 128)
  , dbTypeTest "Lazy Text" $ Data.Text.Lazy.fromStrict <$> Gen.text (Range.linear 0 10) Gen.unicode
  , dbTypeTest "LocalTime" genLocalTime
  , dbTypeTest "Scientific" $ (/10) . fromIntegral @Int @Scientific <$> Gen.integral (Range.linear (-100) 100)
  , dbTypeTest "Text" $ Gen.text (Range.linear 0 10) Gen.unicode
  , dbTypeTest "TimeOfDay" genTimeOfDay
  , dbTypeTest "UTCTime" $ UTCTime <$> genDay <*> genDiffTime
  , dbTypeTest "UUID" $ Data.UUID.fromWords <$> genWord32 <*> genWord32 <*> genWord32 <*> genWord32
  ]

  where
    dbTypeTest ::
      ( Eq a, Show a
      , Rel8.Sql Rel8.DBType a
      , Rel8.IsMaybe a ~ 'False
      , Rel8.Serializable (Rel8.Expr a) a
      , Rel8.Serializable (Rel8.Expr (Maybe a)) (Maybe a)
      ) => TestName -> Gen a -> TestTree
    dbTypeTest name generator = testGroup name
      [ databasePropertyTest name (t (==) generator) getTestDatabase
      , databasePropertyTest ("Maybe " <> name) (t (==) (Gen.maybe generator)) getTestDatabase
      ]

    t :: forall a b. (Show a, Rel8.Sql Rel8.DBType a, Rel8.Serializable (Rel8.Expr a) a)
      => (a -> a -> Bool)
      -> Gen a
      -> ((Connection -> TestT IO ()) -> PropertyT IO b)
      -> PropertyT IO b
    t eq generator transaction = do
      x <- forAll generator

      transaction \connection -> do
        [res] <- liftIO $ Rel8.select connection $ pure (Rel8.lit x :: Rel8.Expr a)
        diff res eq x

    genDay :: Gen Day
    genDay = do
      year <- Gen.integral (Range.linear 1970 3000)
      month <- Gen.integral (Range.linear 1 12)
      day <- Gen.integral (Range.linear 1 31)
      Gen.just $ pure $ fromGregorianValid year month day

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


testDBEq :: IO TmpPostgres.DB -> TestTree
testDBEq getTestDatabase = testGroup "DBEq instances"
  [ dbEqTest "Bool" Gen.bool
  , dbEqTest "Int32" $ Gen.integral @_ @Int32 Range.linearBounded
  , dbEqTest "Int64" $ Gen.integral @_ @Int64 Range.linearBounded
  , dbEqTest "Text" $ Gen.text (Range.linear 0 10) Gen.unicode
  ]

  where
    dbEqTest ::
      ( Eq a, Show a
      , Rel8.Sql Rel8.DBEq a
      , Rel8.IsMaybe a ~ 'False
      , Rel8.Serializable (Rel8.Expr a) a
      , Rel8.Serializable (Rel8.Expr (Maybe a)) (Maybe a)
      ) => TestName -> Gen a -> TestTree
    dbEqTest name generator = testGroup name
      [ databasePropertyTest name (t generator) getTestDatabase
      , databasePropertyTest ("Maybe " <> name) (t (Gen.maybe generator)) getTestDatabase
      ]

    t :: forall a.
      ( Eq a, Show a
      , Rel8.Sql Rel8.DBEq a
      , Rel8.Serializable (Rel8.Expr a) a
      )
      => Gen a
      -> ((Connection -> TestT IO ()) -> PropertyT IO ())
      -> PropertyT IO ()
    t generator transaction = do
      (x, y) <- forAll (liftA2 (,) generator generator)

      transaction \connection -> do
        [res] <- liftIO $ Rel8.select connection $ pure $ Rel8.lit @(Rel8.Expr a) x Rel8.==. Rel8.lit y
        res === (x == y)

        cover 1 "Equal" $ x == y
        cover 1 "Not Equal" $ x /= y


testTableEquality :: IO TmpPostgres.DB -> TestTree
testTableEquality = databasePropertyTest "TestTable equality" \transaction -> do
   (x, y) <- forAll $ liftA2 (,) genTestTable genTestTable

   transaction \connection -> do
     [eq] <- liftIO $ Rel8.select connection do
       pure $ Rel8.lit x Rel8.==: Rel8.lit y

     eq === (x == y)

     cover 1 "Equal" $ x == y
     cover 1 "Not Equal" $ x /= y


testFromString :: IO TmpPostgres.DB -> TestTree
testFromString = databasePropertyTest "FromString" \transaction -> do
  str <- forAll $ Gen.list (Range.linear 0 10) Gen.unicode

  transaction \connection -> do
    [result] <- liftIO $ Rel8.select connection $ pure $ fromString str
    result === pack str


testCatMaybeTable :: IO TmpPostgres.DB -> TestTree
testCatMaybeTable = databasePropertyTest "catMaybeTable" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) genTestTable

  transaction \connection -> do
    selected <- liftIO $ Rel8.select connection do
      testTable <- Rel8.values $ Rel8.lit <$> rows
      Rel8.catMaybeTable $ Rel8.bool Rel8.nothingTable (pure testTable) (testTableColumn2 testTable)

    sort selected === sort (filter testTableColumn2 rows)


testCatMaybe :: IO TmpPostgres.DB -> TestTree
testCatMaybe = databasePropertyTest "catMaybe" \transaction -> evalM do
  rows <- forAll $ Gen.list (Range.linear 0 10) $ Gen.maybe Gen.bool

  transaction \connection -> do
    selected <- evalM $ liftIO $ Rel8.select connection do
      Rel8.catNullable =<< Rel8.values (map Rel8.lit rows)

    sort selected === sort (catMaybes rows)


testMaybeTable :: IO TmpPostgres.DB -> TestTree
testMaybeTable = databasePropertyTest "maybeTable" \transaction -> evalM do
  (rows, def) <- forAll $ liftA2 (,) (Gen.list (Range.linear 0 10) genTestTable) genTestTable

  transaction \connection -> do
    selected <- liftIO $ Rel8.select connection $
      Rel8.maybeTable (Rel8.lit def) id <$> Rel8.optional (Rel8.values (Rel8.lit <$> rows))

    case rows of
      [] -> selected === [def]
      _ -> sort selected === sort rows


data TwoTestTables f =
  TwoTestTables
    { testTable1 :: TestTable f
    , testTable2 :: TestTable f
    }
  deriving stock Generic 
  deriving anyclass Rel8.Rel8able


deriving stock instance Eq (TwoTestTables Identity)
deriving stock instance Ord (TwoTestTables Identity)
deriving stock instance Show (TwoTestTables Identity)


testNestedTables :: IO TmpPostgres.DB -> TestTree
testNestedTables = databasePropertyTest "Nested TestTables" \transaction -> evalM do
  rows <- forAll do
    Gen.list (Range.linear 0 10) $
      liftA2 TwoTestTables genTestTable genTestTable

  transaction \connection -> do
    selected <- liftIO $ Rel8.select connection do
      Rel8.values (Rel8.lit <$> rows)

    sort selected === sort rows


testMaybeTableApplicative :: IO TmpPostgres.DB -> TestTree
testMaybeTableApplicative = databasePropertyTest "MaybeTable (<*>)" \transaction -> evalM do
  rows1 <- genRows
  rows2 <- genRows

  transaction \connection -> do
    selected <- liftIO $ Rel8.select connection do
      as <- Rel8.optional (Rel8.values (Rel8.lit <$> rows1))
      bs <- Rel8.optional (Rel8.values (Rel8.lit <$> rows2))
      pure $ liftA2 (,) as bs

    case (rows1, rows2) of
      ([], []) -> selected === [Nothing]
      ([], bs) -> selected === (Nothing <$ bs)
      (as, []) -> selected === (Nothing <$ as)
      (as, bs) -> sort selected === sort (Just <$> liftA2 (,) as bs)
  where
    genRows :: PropertyT IO [TestTable Identity]
    genRows = forAll do
      Gen.list (Range.linear 0 10) $ liftA2 TestTable (Gen.text (Range.linear 0 10) Gen.unicode) (pure True)

rollingBack
  :: (MonadBaseControl IO m, MonadIO m)
  => Connection -> m a -> m a
rollingBack connection =
  bracket_ 
    (liftIO (run (sql "BEGIN") connection)) 
    (liftIO (run (sql "ROLLBACK") connection))


genTestTable :: Gen (TestTable Identity)
genTestTable = do
  testTableColumn1 <- Gen.text (Range.linear 0 5) Gen.alphaNum
  testTableColumn2 <- Gen.bool
  return TestTable{..}


testUpdate :: IO TmpPostgres.DB -> TestTree
testUpdate = databasePropertyTest "Can UPDATE TestTable" \transaction -> do
  rows <- forAll $ Gen.map (Range.linear 0 5) $ liftA2 (,) genTestTable genTestTable

  transaction \connection -> do
    void $ liftIO $ Rel8.insert connection
      Rel8.Insert
        { into = testTableSchema
        , rows = map (Rel8.toInsert . Rel8.lit) $ Map.keys rows
        , onConflict = Rel8.DoNothing
        , returning = Rel8.NumberOfRowsAffected
        }

    void $ liftIO $ Rel8.update connection
      Rel8.Update
        { target = testTableSchema
        , set = \r ->
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
        , updateWhere = \_ -> Rel8.lit True
        , returning = Rel8.NumberOfRowsAffected
        }

    selected <- liftIO $ Rel8.select connection do
      Rel8.each testTableSchema

    sort selected === sort (Map.elems rows)

    cover 1 "Empty" $ null rows
    cover 1 "Singleton" $ null $ drop 1 $ Map.keys rows
    cover 1 ">1 row" $ not $ null $ drop 1 $ Map.keys rows


testDelete :: IO TmpPostgres.DB -> TestTree
testDelete = databasePropertyTest "Can DELETE TestTable" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 5) genTestTable

  transaction \connection -> do
    void $ liftIO $ Rel8.insert connection
      Rel8.Insert
        { into = testTableSchema
        , rows = map (Rel8.toInsert . Rel8.lit) rows
        , onConflict = Rel8.DoNothing
        , returning = Rel8.NumberOfRowsAffected
        }

    deleted <-
      liftIO $ Rel8.delete connection
        Rel8.Delete
          { from = testTableSchema
          , deleteWhere = testTableColumn2
          , returning = Rel8.Projection id
          }

    selected <- liftIO $ Rel8.select connection do
      Rel8.each testTableSchema

    sort (deleted <> selected) === sort rows


newtype HKNestedPair f = HKNestedPair { pairOne :: (TestTable f, TestTable f) }
  deriving stock Generic
  deriving anyclass Rel8.Rel8able

deriving stock instance Eq (HKNestedPair Identity)
deriving stock instance Ord (HKNestedPair Identity)
deriving stock instance Show (HKNestedPair Identity)


testSelectNestedPairs :: IO TmpPostgres.DB -> TestTree
testSelectNestedPairs = databasePropertyTest "Can SELECT nested pairs" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 0 10) $ HKNestedPair <$> liftA2 (,) genTestTable genTestTable

  transaction \connection -> do
    selected <- liftIO $ Rel8.select connection do
      Rel8.values $ map Rel8.lit rows

    sort selected === sort rows


testSelectArray :: IO TmpPostgres.DB -> TestTree
testSelectArray = databasePropertyTest "Can SELECT Arrays (with aggregation)" \transaction -> do
  rows <- forAll $ Gen.list (Range.linear 1 10) Gen.bool

  transaction \connection -> do
    selected <- liftIO $ Rel8.select connection $ Rel8.aggregate do
      Rel8.listAgg <$> Rel8.values (map Rel8.lit rows)

    selected === [foldMap pure rows]


data NestedMaybeTable f = NestedMaybeTable
  { nmt1 :: Rel8.Column f Bool
  , nmt2 :: Rel8.HMaybe f (TestTable f)
  }
  deriving stock Generic
  deriving anyclass Rel8.Rel8able


deriving stock instance Eq (NestedMaybeTable Identity)
deriving stock instance Ord (NestedMaybeTable Identity)
deriving stock instance Show (NestedMaybeTable Identity)


testNestedMaybeTable :: IO TmpPostgres.DB -> TestTree
testNestedMaybeTable = databasePropertyTest "Can nest MaybeTable within other tables" \transaction -> do
  let example = NestedMaybeTable { nmt1 = True, nmt2 = Just (TestTable "Hi" True) }

  transaction \connection -> do
    selected <- liftIO $ Rel8.select connection do
      x <- Rel8.values [Rel8.lit example]
      pure $ Rel8.maybeTable (Rel8.lit False) (\_ -> Rel8.lit True) (nmt2 x)

    selected === [True]
