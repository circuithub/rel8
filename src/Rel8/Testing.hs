{-# language Arrows #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeSynonymInstances #-}
{-# language TypeInType #-}

{-# options -fplugin=RecordDotPreprocessor #-}

module Rel8.Testing where

import Control.Arrow ( arr, returnA )
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics ( Generic )
import Rel8
import Rel8.EqTable

data MyTable = MyTable { columnA :: Bool, columnB :: Int, columnC :: Maybe Int }
  deriving (Generic, Table, EqTable)


myTable :: TableSchema MyTable
myTable = TableSchema{ tableName = "my_table", schema = genericColumns }


dotTestColumnA :: Query x (Row Bool)
dotTestColumnA = fmap (.columnA) (each myTable)


dotTestColumnB :: Query x (Row Int)
dotTestColumnB = fmap (.columnB) (each myTable)


dotTestColumnC :: Query x (Row (Maybe Int))
dotTestColumnC = fmap (.columnC) (each myTable)


selectTest :: Connection -> IO [MyTable]
selectTest c = select c ( each myTable )


data Part = Part { mfrId :: Int, description :: Maybe String }
  deriving (Generic, Table)

part :: TableSchema Part
part = TableSchema "part" genericColumns


allMfrIds :: Query x (Row Int)
allMfrIds = fmap (.mfrId) (each part)


descs :: Query x (Row (Maybe String))
descs = fmap (.description) (each part)


-- First we define our table type. Unlike all database libraries that I'm aware
-- of, there is nothing special here. The only thing we have to do is derive
-- a Table instance, which can be done generically.

data User = User { username :: String, email :: Maybe String }
  deriving (Generic, Table)


-- To be able to SELECT this table, we need to provide a schema. This can be
-- done generically, provided our type is just a product of single columns.

userSchema :: TableSchema User
userSchema =
  TableSchema { tableName = "user", schema = genericColumns }


-- This lets us construct a query that selects all users:

users :: Query x (Row User)
users = each userSchema


-- Note that our Query produces 'Row User', rather than just 'User'. Unlike
-- other Row types in other libraries, expressions aren't limited to just
-- being single columns, so Row User is a two-column expression, and that's
-- perfectly fine.

-- Row's have a 'HasField' instance, so we can also project single columns
-- just using normal Haskell:

userNames :: Query (Row User) (Row String)
userNames = arr (.username)


-- We can run this to IO, too.
fetchUsers :: Connection -> IO [User]
fetchUsers c = select c users


leftJoinTest :: Query x (Row (Maybe User))
leftJoinTest = fmap toRow $ proc _ -> do
  user1 <- each userSchema -< ()
  optional (proc _user1 -> do
    user2 <- each userSchema -< ()
    where_ -< lit False
    returnA -< user2) -< user1


prepareTest :: Query (Row Bool) (Row Part)
prepareTest = proc x -> do
  user <- each part -< ()
  where_ -< x
  returnA -< user
