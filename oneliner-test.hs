{-# LANGUAGE
      Arrows, DataKinds, DeriveGeneric, FlexibleInstances,
      MultiParamTypeClasses #-}

import Control.Arrow
import Control.Applicative
import Rel8
import OpaleyeStub (Connection)

data TestTable f = TestTable
  { testColumnA :: C f ('Column "a" 'NoDefault 'NotNullable 'PGInteger)
  , testColumnB :: C f ('Column "b" 'HasDefault 'Nullable 'PGInteger)
  } deriving (Generic)

instance BaseTable TestTable where
  tableName _ = "test_table"

instance Table (TestTable Expr) (TestTable QueryResult)

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
    (\l r -> toNullable (testColumnA l) ^/=^ testColumnB r)
    queryTable
    queryTable

testConn :: Connection
testConn = undefined
