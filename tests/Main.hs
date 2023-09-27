{-# language BangPatterns #-}
{-# language BlockArguments #-}
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

module Main
  ( main
  )
where

-- base
import Control.Applicative ( empty, liftA2, liftA3 )
import Control.Exception ( bracket, throwIO )
import Control.Monad ((>=>))
import Data.Bifunctor ( bimap )
import Data.Fixed (Fixed (MkFixed))
import Data.Foldable ( for_ )
import Data.Functor (void)
import Data.Int ( Int32, Int64 )
import Data.List ( nub, sort )
import Data.Maybe ( catMaybes )
import Data.String ( fromString )
import Data.Word (Word32, Word8)
import GHC.Generics ( Generic )
import Prelude hiding (truncate)

-- bytestring
import qualified Data.ByteString.Lazy

-- case-insensitive
import Data.CaseInsensitive ( mk )

-- containers
import Data.Containers.ListUtils ( nubOrdOn )
import qualified Data.Map.Strict as Map

-- hasql
import Hasql.Connection ( Connection, acquire, release )
import Hasql.Session ( sql, run )

-- hasql-transaction
import Hasql.Transaction ( Transaction, condemn, statement )
import qualified Hasql.Transaction.Sessions as Hasql

-- hedgehog
import Hedgehog ( property, (===), forAll, cover, diff, evalM, PropertyT, TestT, test, Gen )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- mmorph
import Control.Monad.Morph ( hoist )

-- network-ip
import Network.IP.Addr (NetAddr, IP, IP4(..), IP6(..), IP46(..), net4Addr, net6Addr, fromNetAddr46, Net4Addr, Net6Addr)
import Data.DoubleWord (Word128(..))

-- rel8
import Rel8 ( Result )
import qualified Rel8

-- scientific
import Data.Scientific ( Scientific )

-- tasty
import Test.Tasty

-- tasty-hedgehog
import Test.Tasty.Hedgehog ( testProperty )

-- text
import Data.Text ( Text, pack, unpack )
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
    , testFromString getTestDatabase
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
    ]

  where

    startTestDatabase = do
      db <- TmpPostgres.start >>= either throwIO return

      bracket (either (error . show) return =<< acquire (TmpPostgres.toConnectionString db)) release \conn -> void do
        flip run conn do
          sql "CREATE EXTENSION citext"
          sql "CREATE TABLE test_table ( column1 text not null, column2 bool not null )"
          sql "CREATE TABLE unique_table ( \"key\" text not null unique, \"value\" text not null )"
          sql "CREATE SEQUENCE test_seq"
          sql "CREATE TYPE composite AS (\"bool\" bool, \"char\" char, \"array\" int4[])"

      return db

    stopTestDatabase = TmpPostgres.stop


connect :: TmpPostgres.DB -> IO Connection
connect = acquire . TmpPostgres.toConnectionString >=> either (maybe empty (fail . unpack . decodeUtf8)) pure


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
        , onConflict = Rel8.DoNothing
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
  , char :: !Char
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
  , dbTypeTest "CI Lazy Text" $ mk . Data.Text.Lazy.fromStrict <$> Gen.text (Range.linear 0 10) Gen.unicode
  , dbTypeTest "CI Text" $ mk <$> Gen.text (Range.linear 0 10) Gen.unicode
  , dbTypeTest "Composite" genComposite
  , dbTypeTest "Day" genDay
  , dbTypeTest "Double" $ (/ 10) . fromIntegral @Int @Double <$> Gen.integral (Range.linear (-100) 100)
  , dbTypeTest "Float" $ (/ 10) . fromIntegral @Int @Float <$> Gen.integral (Range.linear (-100) 100)
  , dbTypeTest "Int32" $ Gen.integral @_ @Int32 Range.linearBounded
  , dbTypeTest "Int64" $ Gen.integral @_ @Int64 Range.linearBounded
  , dbTypeTest "Lazy ByteString" $ Data.ByteString.Lazy.fromStrict <$> Gen.bytes (Range.linear 0 128)
  , dbTypeTest "Lazy Text" $ Data.Text.Lazy.fromStrict <$> Gen.text (Range.linear 0 10) Gen.unicode
  , dbTypeTest "LocalTime" genLocalTime
  , dbTypeTest "Scientific" $ (/ 10) . fromIntegral @Int @Scientific <$> Gen.integral (Range.linear (-100) 100)
  , dbTypeTest "Text" $ Gen.text (Range.linear 0 10) Gen.unicode
  , dbTypeTest "TimeOfDay" genTimeOfDay
  , dbTypeTest "UTCTime" $ UTCTime <$> genDay <*> genDiffTime
  , dbTypeTest "UUID" $ Data.UUID.fromWords <$> genWord32 <*> genWord32 <*> genWord32 <*> genWord32
  , dbTypeTest "INet" genNetAddrIP
  ]

  where
    dbTypeTest :: (Eq a, Show a, Rel8.DBType a, Rel8.ToExprs (Rel8.Expr a) a) => TestName -> Gen a -> TestTree
    dbTypeTest name generator = testGroup name
      [ databasePropertyTest name (t generator) getTestDatabase
      , databasePropertyTest ("Maybe " <> name) (t (Gen.maybe generator)) getTestDatabase
      ]

    t :: forall a b. (Eq a, Show a, Rel8.Sql Rel8.DBType a, Rel8.ToExprs (Rel8.Expr a) a)
      => Gen a
      -> (TestT Transaction () -> PropertyT IO b)
      -> PropertyT IO b
    t generator transaction = do
      x <- forAll generator
      y <- forAll generator

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
            xss <- Rel8.catListTable (Rel8.listTable [Rel8.listTable [Rel8.listTable [Rel8.litExpr x, Rel8.litExpr y]]])
            xs <- Rel8.catListTable xss
            Rel8.catListTable xs
        diff res''' (==) [x, y]

    genComposite :: Gen Composite
    genComposite = do
      bool <- Gen.bool
      char <- Gen.unicode
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

    genWord128 :: Gen Word128
    genWord128 = Gen.integral Range.linearBounded

    genNetAddrIP  :: Gen (NetAddr IP)
    genNetAddrIP =
      let
        genIP4Mask :: Gen Word8
        genIP4Mask = Gen.integral (Range.linearFrom 0 0 32)

        genIPv4 :: Gen (IP46 Net4Addr Net6Addr)
        genIPv4 = IPv4 <$> (liftA2 net4Addr (IP4 <$> genWord32) genIP4Mask)

        genIP6Mask :: Gen Word8
        genIP6Mask = Gen.integral (Range.linearFrom 0 0 128)

        genIPv6 :: Gen (IP46 Net4Addr Net6Addr)
        genIPv6 = IPv6 <$> (liftA2 net6Addr (IP6 <$> genWord128) genIP6Mask)

       in fromNetAddr46 <$> Gen.choice [ genIPv4, genIPv6 ]


testDBEq :: IO TmpPostgres.DB -> TestTree
testDBEq getTestDatabase = testGroup "DBEq instances"
  [ dbEqTest "Bool" Gen.bool
  , dbEqTest "Int32" $ Gen.integral @_ @Int32 Range.linearBounded
  , dbEqTest "Int64" $ Gen.integral @_ @Int64 Range.linearBounded
  , dbEqTest "Text" $ Gen.text (Range.linear 0 10) Gen.unicode
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


testTableEquality :: IO TmpPostgres.DB -> TestTree
testTableEquality = databasePropertyTest "TestTable equality" \transaction -> do
   (x, y) <- forAll $ liftA2 (,) genTestTable genTestTable

   transaction do
     eq <- lift do
       statement () $ Rel8.run1 $ Rel8.select do
         pure $ Rel8.lit x Rel8.==: Rel8.lit y

     eq === (x == y)


testFromString :: IO TmpPostgres.DB -> TestTree
testFromString = databasePropertyTest "FromString" \transaction -> do
  str <- forAll $ Gen.list (Range.linear 0 10) Gen.unicode

  transaction do
    result <- lift do
      statement () $ Rel8.run1 $ Rel8.select do
        pure $ fromString str
    result === pack str


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
      Gen.list (Range.linear 0 10) $ liftA2 TestTable (Gen.text (Range.linear 0 10) Gen.unicode) (pure True)


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
        , onConflict = Rel8.DoNothing
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
        , onConflict = Rel8.DoNothing
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
                , onConflict = Rel8.DoNothing
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
                , onConflict = Rel8.DoNothing
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
                , onConflict = Rel8.DoNothing
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
        , onConflict = Rel8.DoNothing
        , returning = Rel8.NoReturning
        }

      statement () $ Rel8.run_ $ Rel8.insert Rel8.Insert
        { into = uniqueTableSchema
        , rows = Rel8.values $ Rel8.lit <$> bs
        , onConflict = Rel8.DoUpdate Rel8.Upsert
            { index = uniqueTableKey
            , predicate = Nothing
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
