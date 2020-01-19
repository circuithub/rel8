{-# language ApplicativeDo #-}
{-# language LambdaCase #-}
{-# language TypeOperators #-}
{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

{-# options -fno-warn-missing-signatures #-}

module Rel8.Tests where

import Data.Int
import Rel8

-- TODO Users should not need these imports
import Rel8.Column
import Rel8.Table


-- TODO Part of generic derivation
data Refl a b where
  Refl :: Refl a a


data ( l & r ) a =
  L ( l a ) | R ( r a )



data Part f =
  Part
    { partId :: Column f Int32
    , partName :: Column f String
    }


-- TODO Generically derive.
instance HigherKindedTable Part where
  type HConstrainTraverse Part c = ( c Int32, c String )
  data HField Part x where
    PartId :: HField Part Int32
    PartName :: HField Part String
  hfield Part{ partId } PartId = C partId
  hfield Part{ partName } PartName = C partName
  htabulate _ f =
    Part <$> do toColumn <$> f PartId
         <*> do toColumn <$> f PartName
  -- htraverseTableWithIndexC f Part{ partId, partName } = do
  --   partId' <-
  --     f ( L Refl ) ( C partId )

  --   partName' <-
  --     f ( R Refl ) ( C partName )

  --   pure ( tabulate \case ( L Refl ) -> partId'
  --                         ( R Refl ) -> partName'
  --        )



-- TODO Maybe provide a generic version?
parts :: TableSchema ( Part ColumnSchema )
parts =
  TableSchema
    { tableName = "part"
    , tableSchema = Nothing
    , tableColumns = Part { partId = "part_id"
                          , partName = "part_name"
                          }
    }


allParts :: MonadQuery m => m ( Part ( Expr m ) )
allParts =
  each parts


-- partsEq :: MonadQuery m => m ( Expr m Bool )
-- partsEq = do
--   parts1 <- allParts
--   parts2 <- allParts
--   return (parts1 ==. parts2)



-- -- select_allParts :: m [ Part Identity ]
-- -- select_allParts =
-- --   select allParts


-- -- TODO Can we make this infer?
-- -- allParts_inferred =
-- --   each parts


-- allPartIds :: MonadQuery m => m ( Expr m Int32 )
-- allPartIds =
--   partId <$> allParts


-- -- selectAllPartIds :: IO [ Int32 ]
-- -- selectAllPartIds =
-- --   select allPartIds


-- data Project f =
--   Project
--     { projectId :: Column f Int32
--     }


-- -- TODO Generically derive.
-- instance HigherKindedTable Project where
--   type HConstrainTraverse Project c =
--     c Int32


-- projects :: TableSchema ( Project ColumnSchema )
-- projects =
--   TableSchema
--     { tableName = "project"
--     , tableSchema = Nothing
--     , tableColumns = Project { projectId = "id" }
--     }


-- data ProjectPart f =
--   ProjectPart
--     { projectPartProjectId :: Column f Int32
--     , projectPartPartId :: Column f Int32
--     }


-- -- TODO Generically derive.
-- instance HigherKindedTable ProjectPart where
--   type HConstrainTraverse ProjectPart c=
--     c Int32


-- projectParts :: TableSchema ( ProjectPart ColumnSchema )
-- projectParts =
--   TableSchema
--     { tableName = "project_part"
--     , tableSchema = Nothing
--     , tableColumns = ProjectPart { projectPartPartId = "part_id"
--                                  , projectPartProjectId = "project_id"
--                                  }
--     }


-- leftJoinTest :: MonadQuery m => m ( Expr m Int32, MaybeTable ( ProjectPart ( Expr m ) ) ( Expr m ) )
-- leftJoinTest = do
--   Part{ partId } <-
--     each parts

--   projectPart <-
--     leftJoin ( each projectParts ) \ProjectPart{ projectPartPartId } ->
--       projectPartPartId ==. partId

--   return ( partId, projectPart )


-- data PartWithProject f =
--   PartWithProject
--     { part :: Part f
--     , project :: Project f
--     }


-- -- TODO Generically derive.
-- instance HigherKindedTable PartWithProject where
--   type HConstrainTraverse PartWithProject c =
--     ( HConstrainTraverse Part c, HConstrainTraverse Project c )


-- partsWithProjects :: MonadQuery m => m ( PartWithProject ( Expr m ) )
-- partsWithProjects = do
--   part <-
--     each parts

--   projectPart <-
--     each projectParts

--   where_ ( projectPartPartId projectPart ==. partId part )

--   project <-
--     each projects

--   where_ ( projectPartProjectId projectPart ==. projectId project )

--   return PartWithProject{..}


-- nestedTableEq :: MonadQuery m => m ( Expr m Bool )
-- nestedTableEq = do
--   l <- partsWithProjects
--   r <- partsWithProjects
--   return ( l ==. r )


-- -- select_partsWithProjects =
-- --   select partsWithProjects


-- partsAggregation
--   :: MonadQuery m
--   => m ( Expr m String, Sum ( Expr m Int32 ) )
-- partsAggregation = do
--   groupAndAggregate
--     ( \part -> GroupBy ( partName part ) ( Sum ( partId part ) ) )
--     allParts


-- -- illegalPartsAggregation1 :: MonadQuery m => m ( GroupBy ( Expr m String ) ( Sum ( Expr m Int32 ) ) )
-- -- illegalPartsAggregation1 = do
-- --   unreachable <- allParts

-- --   groupAndAggregate
-- --     ( \part -> GroupBy ( partName unreachable ) ( Sum ( partId part ) ) )
-- --     allParts


-- -- illegalPartsAggregation2 :: MonadQuery m => m ( GroupBy ( Expr m String ) ( Sum ( Expr m Int32 ) ) )
-- -- illegalPartsAggregation2 = do
-- --   unreachable <- allParts

-- --   groupAndAggregate
-- --     ( \part -> unreachable )
-- --     allParts
