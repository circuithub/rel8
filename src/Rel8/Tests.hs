{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Rel8.Tests where

import Data.Functor.Identity ( Identity )
import Data.Int
import Data.Monoid
import Database.PostgreSQL.Simple ( Connection )
import GHC.Generics
import Rel8


data Part f =
  Part
    { partId :: Column f Int64
    , partName :: Column f String
    }
  deriving
    ( Generic, HigherKindedTable )



-- TODO Maybe provide a generic version?
parts :: TableSchema ( Part ColumnSchema )
parts =
  TableSchema
    { tableName = "part"
    , tableSchema = Nothing
    , tableColumns = Part { partId = "id"
                          , partName = "urn"
                          }
    }


allParts :: Query ( Part Expr )
allParts =
  each parts


proj1 :: Query ( Expr Int64 )
proj1 = partId <$> allParts


partsEq :: Query ( Expr Bool )
partsEq = do
  parts1 <- allParts
  parts2 <- allParts
  return (parts1 ==. parts2)



select_allParts :: Connection -> IO [ Part Identity ]
select_allParts c =
  select c allParts


proj2 :: Connection -> IO [ Int64 ]
proj2 c = map partId <$> select_allParts c


-- -- TODO Can we make this infer?
-- allParts_inferred =
--   each parts


allPartIds :: Query ( Expr Int64 )
allPartIds =
  partId <$> allParts


selectAllPartIds :: Connection -> IO [ Int64 ]
selectAllPartIds c =
  select c allPartIds


data Project f =
  Project
    { projectId :: Column f Int64
    }
  deriving
    ( Generic, HigherKindedTable )


projects :: TableSchema ( Project ColumnSchema )
projects =
  TableSchema
    { tableName = "project"
    , tableSchema = Nothing
    , tableColumns = Project { projectId = "id" }
    }


data ProjectPart f =
  ProjectPart
    { projectPartProjectId :: Column f Int64
    , projectPartPartId :: Column f Int64
    }
  deriving
    ( Generic, HigherKindedTable )


projectParts :: TableSchema ( ProjectPart ColumnSchema )
projectParts =
  TableSchema
    { tableName = "project_part"
    , tableSchema = Nothing
    , tableColumns = ProjectPart { projectPartPartId = "part_id"
                                 , projectPartProjectId = "project_id"
                                 }
    }


leftJoinTest :: Query ( Expr ( Maybe Int64 ) )
leftJoinTest = do
  Part{ partId = partId1 } <-
    each parts

  projectPart <- optional do
    part2 <- each parts
    where_ $ partId part2 ==. partId1
    return part2

  return $ maybeTable ( lit Nothing ) ( liftNull . partId ) projectPart


data PartWithProject f =
  PartWithProject
    { part :: Part f
    , project :: Project f
    }
  deriving
    ( Generic, HigherKindedTable )


partsWithProjects :: Query ( PartWithProject Expr )
partsWithProjects = do
  part <-
    each parts

  projectPart <-
    each projectParts

  where_ ( projectPartPartId projectPart ==. partId part )

  project <-
    each projects

  where_ ( projectPartProjectId projectPart ==. projectId project )

  return PartWithProject{..}


nestedTableEq :: Query ( Expr Bool )
nestedTableEq = do
  l <- partsWithProjects
  r <- partsWithProjects
  return ( l ==. r )


-- select_partsWithProjects =
--   select partsWithProjects


partsAggregation
  :: Query ( Expr String, Sum ( Expr Int64 ) )
partsAggregation = do
  groupAndAggregate
    ( \part -> GroupBy ( partName part ) ( Sum ( partId part ) ) )
    allParts


-- illegalPartsAggregation1 :: MonadQuery m => m ( GroupBy ( Expr m String ) ( Sum ( Expr m Int64 ) ) )
-- illegalPartsAggregation1 = do
--   unreachable <- allParts

--   groupAndAggregate
--     ( \part -> GroupBy ( partName unreachable ) ( Sum ( partId part ) ) )
--     allParts


-- illegalPartsAggregation2 :: MonadQuery m => m ( GroupBy ( Expr m String ) ( Sum ( Expr m Int64 ) ) )
-- illegalPartsAggregation2 = do
--   unreachable <- allParts

--   groupAndAggregate
--     ( \part -> unreachable )
--     allParts


data HasNull f =
  HasNull { nullId :: Column f ( Maybe Int64 )
          , notNullId :: Column f Int64
          }
  deriving
    ( Generic, HigherKindedTable )


hasNull :: TableSchema ( HasNull ColumnSchema )
hasNull =
  TableSchema
    { tableName = "has_null"
    , tableSchema = Nothing
    , tableColumns = HasNull { nullId = "test", notNullId = "not_null" }
    }


nullTest :: Query ( HasNull Expr )
nullTest = do
  HasNull{ nullId, notNullId } <-
    each hasNull

  where_ ( null_ ( lit False ) ( lit 42 ==. ) nullId )

  return HasNull{ nullId, notNullId }


-- nullTestLeftJoin
--   :: MonadQuery m
--   => m ( Expr ( Maybe Int64 ), Expr ( Maybe Int64 ) )
-- nullTestLeftJoin = do
--   t1 <-
--     each hasNull

--   t2 <-
--     leftJoin ( each hasNull ) \HasNull{ nullId } ->
--       null_ ( lit False ) ( notNullId t1 ==. ) nullId

--   return ( nullId ( maybeTable t2 ), notNullId ( maybeTable t2 ) )


-- nullTestLeftJoinEasyEq
--   :: MonadQuery m
--   => m ( Expr ( Maybe Int64 ), Expr ( Maybe Int64 ) )
-- nullTestLeftJoinEasyEq = do
--   t1 <-
--     each hasNull

--   t2 <-
--     leftJoin ( each hasNull ) \HasNull{ nullId } ->
--       nullId ==. liftNull ( notNullId t1 )

--   return ( nullId ( maybeTable t2 ), notNullId ( maybeTable t2 ) )


-- maybeTableQ :: MonadQuery m => m ( MaybeTable ( HasNull Expr ) )
-- maybeTableQ = do
--   t1 <-
--     each hasNull

--   leftJoin ( each hasNull ) \HasNull{ nullId } ->
--     nullId ==. liftNull ( notNullId t1 )


-- select_maybeTable :: Connection -> IO [ Maybe ( HasNull Identity ) ]
-- select_maybeTable c =
--   select c maybeTableQ


-- catNullsTest :: Query ( Expr Int64 )
-- catNullsTest =
--   catNulls ( nullId <$> each hasNull )


unionTest :: Query ( Part Expr )
unionTest =
  union ( each parts ) ( each parts )
