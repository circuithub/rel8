{-# LANGUAGE
      Arrows, DataKinds, DeriveGeneric, FlexibleInstances,
      MultiParamTypeClasses, OverloadedStrings #-}

{-# LANGUAGE RecordWildCards, StandaloneDeriving, ApplicativeDo #-}

import Data.Int
import Prelude hiding (not, (/=), (==))
import Control.Arrow
import Control.Applicative
import Rel8
import Database.PostgreSQL.Simple
import System.IO.Unsafe
import qualified Streaming.Prelude as S
import Data.Text (Text)

data Part f = Part
  { partId :: C f "id" 'HasDefault Int
  , partDescription :: C f "description" 'NoDefault (Maybe Text)
  } deriving (Generic)

instance BaseTable "part" Part
deriving instance Show (Part QueryResult)

allParts :: Stream (Of (Part QueryResult)) IO ()
allParts = select testConn queryTable

testQueryApDo :: Stream (Of (Part QueryResult, Part QueryResult)) IO ()
testQueryApDo = select testConn $
  do l <- queryTable
     r <- queryTable
    return (l,r)

test1Col :: Stream (Of (Col Int)) IO ()
test1Col = select testConn $
  proc _ -> do
    t <- queryTable -< ()
    returnA -< partId t

testLeftJoin :: Stream (Of (Part QueryResult, Col (Maybe Int))) IO ()
testLeftJoin =
  select testConn $
  fmap (\(l,r) -> (l, r ? partId)) $
  leftJoin
    (\l r -> partId l /= partId r)
    queryTable
    queryTable

testAggregate :: Stream (Of (Col Bool, Col Int64)) IO ()
testAggregate = select testConn $ aggregate $ proc _ -> do
  Part {..} <- queryTable -< ()
  returnA -< (groupBy (lit True), count partId)

testConn :: Connection
testConn =
  unsafePerformIO $
  connect
    defaultConnectInfo
    {connectUser = "circuithub", connectDatabase = "circuithub"}

testInsert :: IO Int64
testInsert =
  insert testConn
         [Part {partId = InsertDefault
               ,partDescription = lit (Just "Hello")}]

testUpdate :: IO Int64
testUpdate = update testConn (\Part{..} -> partId == lit 9538091) id

testWhere :: Stream (Of (Col Int)) IO ()
testWhere = select testConn $ do
  Part{..} <- queryTable -< ()
  where_ -< isNull partDescription
  return partId
