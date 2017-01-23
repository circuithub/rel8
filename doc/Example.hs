-- This is the example from the documentation. We don't run any tests,
-- just compiling is deemed satisfactory. If this fails to compile,
-- make sure to update the documentation with the necessary changes!

{-# LANGUAGE Arrows, DataKinds, DeriveGeneric, FlexibleInstances,
             OverloadedStrings #-}

import Control.Applicative
import Control.Arrow
import Rel8

data Part f =
  Part { partId     :: C f "PID" 'HasDefault Int
       , partName   :: C f "PName" 'NoDefault String
       , partColor  :: C f "Color" 'NoDefault Int
       , partWeight :: C f "Weight" 'NoDefault Double
       , partCity   :: C f "City" 'NoDefault String
       } deriving (Generic)

instance BaseTable Part where tableName = "part"

allParts :: Query (Part Expr)
allParts = queryTable

allPartCities :: Query (Expr String)
allPartCities = partCity <$> allParts

londonParts :: Query (Part Expr)
londonParts = filterQuery (\p -> partCity p ==. "London") allParts

heavyParts :: Query (Part Expr)
heavyParts = proc _ -> do
  part <- queryTable -< ()
  where_ -< partWeight part >. 5
  returnA -< part

--------------------------------------------------------------------------------
main :: IO ()
main = return ()
