{-# LANGUAGE
      Arrows, DataKinds, DeriveGeneric, FlexibleInstances,
      MultiParamTypeClasses #-}

{-# LANGUAGE RecordWildCards #-}

import Data.Int
import Prelude hiding (not, (/=))
import Control.Arrow
import Control.Applicative
import Rel8
import OpaleyeStub (Connection)

data TestTable f = TestTable
  { testColumnA :: C f "a" 'NoDefault Int
  , testColumnB :: C f "b" 'HasDefault (Maybe Int)
  } deriving (Generic)

instance BaseTable "Part" TestTable

testQuery :: Stream (Of (TestTable QueryResult, TestTable QueryResult)) IO ()
testQuery = select testConn $
  proc _ -> do
    (l,r) <- liftA2 (,) queryTable queryTable -< ()
    returnA -< (l,r)

test1Col :: Stream (Of (Col Int)) IO ()
test1Col = select testConn $
  proc _ -> do
    t <- queryTable -< ()
    returnA -< testColumnA t

testLeftJoin :: Stream (Of (TestTable QueryResult, Col (Maybe Int))) IO ()
testLeftJoin =
  select testConn $
  fmap (\(l,r) -> (l, r ? testColumnA)) $
  leftJoin
    (\l r -> toNullable (testColumnA l) /= testColumnB r)
    queryTable
    queryTable

testAggregate :: Stream (Of (Col Int, Col Int64)) IO ()
testAggregate = select testConn $ aggregate $ proc _ -> do
  TestTable{..} <- queryTable -< ()
  returnA -< (groupBy testColumnA, count testColumnB)

testConn :: Connection
testConn = undefined
