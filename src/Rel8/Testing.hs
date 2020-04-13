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

import GHC.Generics ( Generic )
import Rel8 ( Expr, Table, Schema, genericSchema, each )
import Rel8.IO ( select )


data MyTable = MyTable { columnA :: Bool, columnB :: Int, columnC :: Int }
  deriving (Generic, Table)


myTable :: Schema MyTable
myTable = genericSchema


dotTestColumnA :: Expr Bool
dotTestColumnA = (undefined :: Expr MyTable).columnA


dotTestColumnB :: Expr Int
dotTestColumnB = (undefined :: Expr MyTable).columnB


dotTestColumnC :: Expr Int
dotTestColumnC = (undefined :: Expr MyTable).columnC


selectTest :: IO [MyTable]
selectTest = select undefined ( each myTable )
