{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
-- This is the example from the documentation. We don't run any tests,
-- just compiling is deemed satisfactory. If this fails to compile,
-- make sure to update the documentation with the necessary changes!

{-# LANGUAGE Arrows, DataKinds, DeriveGeneric, FlexibleInstances,
             OverloadedStrings, MultiParamTypeClasses #-}

module Main where

import Control.Applicative
import Control.Arrow
import Rel8
import Data.Int

data Small f =
  Small { smallId     :: C f "PID" 'HasDefault Int32
       , smallName   :: C f "PName" 'NoDefault String
       -- , partColor  :: C f "Color" 'NoDefault Int32
       -- , partWeight :: C f "Weight" 'NoDefault Double
       -- , partCity   :: C f "City" 'NoDefault String
       } deriving (Generic, HigherKindedTable)

instance BaseTable Small where
  tableName = "small"
  -- tabular DomExpr =
  --   iso
  --     (\s@(Small a b) -> Pair (Identity (Colimit a)) (Identity (Colimit b)))
  --     (\(Pair (Identity (Colimit (Expr a))) (Identity (Colimit (Expr b)))) -> Small (Expr a) (Expr b))
  -- tabular DomSchemaInfo =
  --   iso
  --     (\s@(Small a b) -> Pair (Identity (Colimit a)) (Identity (Colimit b)))
  --     (\(Pair (Identity (Colimit (Expr a))) (Identity (Colimit (Expr b)))) -> Small (Expr a) (Expr b))


data Part f =
  Part { partId     :: C f "PID" 'HasDefault Int32
       , partName   :: C f "PName" 'NoDefault String
       , partColor  :: C f "Color" 'NoDefault Int32
       , partWeight :: C f "Weight" 'NoDefault Double
       , partCity   :: C f "City" 'NoDefault String
       } deriving (Generic, HigherKindedTable)

instance BaseTable Part where tableName = "part"
deriving instance Show (Part QueryResult)
deriving instance Show (Part SchemaInfo)

allParts :: Query (Part Expr)
allParts = queryTable

allPartCities :: Query (Expr String)
allPartCities = partCity <$> allParts

londonParts :: Query (Part Expr)
londonParts = filterQuery (\p -> partCity p ==. "London") allParts

data Supplier f = Supplier
  { supplierId :: C f "SID" 'HasDefault Int32
  , supplierName :: C f "SName" 'NoDefault String
  , supplierStatus :: C f "Status" 'NoDefault Int32
  , supplierCity :: C f "City" 'NoDefault String
  } deriving (Generic, HigherKindedTable)

instance BaseTable Supplier where tableName = "supplier"
deriving instance Show (Supplier QueryResult)

allPartsAndSuppliers :: Query (Part Expr, Supplier Expr)
allPartsAndSuppliers = proc _ -> do
  part <- queryTable -< ()
  supplier <- queryTable -< ()
  returnA -< (part, supplier)

allPartsAndSuppliers2 :: Query (Part Expr, Supplier Expr)
allPartsAndSuppliers2 = liftA2 (,) queryTable queryTable

partsAndSuppliers :: Query (Part Expr, Supplier Expr)
partsAndSuppliers =
  filterQuery
    (\(part, supplier) -> partCity part ==. supplierCity supplier)
    allPartsAndSuppliers

--------------------------------------------------------------------------------
main :: IO ()
main = return ()
