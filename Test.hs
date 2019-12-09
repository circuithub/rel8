{-# language DataKinds #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}

module Test where

import HList
import Rel8

data TestTable f =
  TestTable
    { columnA :: f Int
    , columnB :: f Bool
    }


instance Table TestTable where
  type Schema TestTable = '[ Int, Bool ]

  fromHList ( HCons x ( HCons y HNil ) ) =
    TestTable x y

  toHList ( TestTable x y ) =
    HCons x ( HCons y HNil )


testTable :: TableSchema TestTable
testTable =
  TableSchema
    { tableName = "test_table"
    , tableSchema =
        TestTable
          { columnA = "column_a"
          , columnB = "column_b"
          }
    }


test :: IO ()
test = do
  select $ limit 5 $ offset 10 $ do
    TestTable{ columnA, columnB } <-
      table testTable

    Rel8.foldMap
      ( \TestTable{ columnA, columnB } ->
          C ( countDistinct columnA )
      )
      ( table testTable )

    where_ columnB

    return ( C columnB :& C columnA )

  return ()
