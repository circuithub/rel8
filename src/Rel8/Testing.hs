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

import Database.PostgreSQL.Simple (Connection)
import GHC.Generics ( Generic )
import Rel8 ( Expr, Query, Table, Schema(..), genericColumns, each )
import Rel8.IO ( select )

data MyTable = MyTable { columnA :: Bool, columnB :: Int } -- Adding this will fail, as 'Maybe Int' has no schema: , columnC :: Maybe Int }
  deriving (Generic, Table)


myTable :: Schema MyTable
myTable = Schema{ tableName = "my_table", schema = genericColumns }


dotTestColumnA :: Query x (Expr Bool)
dotTestColumnA = fmap (.columnA) (each myTable)


dotTestColumnB :: Query x (Expr Int)
dotTestColumnB = fmap (.columnB) (each myTable)


-- dotTestColumnC :: Expr (Maybe Int)
-- dotTestColumnC = fmap (.columnC) (each myTable)


selectTest :: Connection -> IO [MyTable]
selectTest c = select c ( each myTable )


data Part = Part { mfrId :: Int }
  deriving (Generic, Table, Show)

part :: Schema Part
part = Schema "part" genericColumns
