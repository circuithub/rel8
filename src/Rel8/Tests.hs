{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
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

import Data.Functor.Identity
import Rel8.Column
import Rel8.ColumnSchema
import Rel8.EqTable
import Rel8.Expr
import Rel8.HigherKinded
import Rel8.MaybeTable
import Rel8.MonadQuery
import Rel8.Query
import Rel8.TableSchema
import Rel8.ZipLeaves


data Part f =
  Part
    { partId :: Column f Int
    , partName :: Column f String
    }


-- TODO Generically derive.
instance HigherKinded Part where
  type ZipRecord Part f g c =
    ( CanZipLeaves ( C f Int ) ( C g Int ) c
    , ZipLeaves ( C f Int ) ( C g Int ) f g
    , CanZipLeaves ( C f String ) ( C g String ) c
    , ZipLeaves ( C f String ) ( C g String ) f g
    )

  zipRecord
    :: forall proxy c f g m
     . ( Applicative m, ZipRecord Part f g c )
    => proxy c
    -> (forall x. c x => C f x -> C f x -> m (C g x))
    -> Part f -> Part f -> m (Part g)
  zipRecord proxy f a b =
    Part
      <$> do toColumn @g @Int <$>
               zipLeaves proxy f (C @f @Int (partId a)) (C (partId b))
      <*> do toColumn @g @String <$>
               zipLeaves proxy f (C @f @String (partName a)) (C (partName b))


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


partsEq :: MonadQuery m => m ( Expr m Bool )
partsEq = do
  parts1 <- allParts
  parts2 <- allParts
  return (parts1 ==. parts2)



select_allParts :: m [ Part Identity ]
select_allParts =
  select allParts


-- TODO Can we make this infer?
-- allParts_inferred =
--   each parts


allPartIds :: MonadQuery m => m ( Expr m Int )
allPartIds =
  partId <$> allParts


selectAllPartIds :: IO [ Int ]
selectAllPartIds =
  select allPartIds


data Project f =
  Project
    { projectId :: Column f Int
    }


-- TODO Generically derive.
instance HigherKinded Project where
  type ZipRecord Project f g c =
    ( CanZipLeaves ( C f Int ) ( C g Int ) c
    , ZipLeaves ( C f Int ) ( C g Int ) f g
    )

  zipRecord
    :: forall proxy c f g m
     . ( Applicative m, ZipRecord Project f g c )
    => proxy c
    -> (forall x. c x => C f x -> C f x -> m (C g x))
    -> Project f -> Project f -> m (Project g)
  zipRecord proxy f a b =
    Project
      <$> do toColumn @g @Int <$>
               zipLeaves proxy f (C @f @Int (projectId a)) (C (projectId b))


projects :: TableSchema ( Project ColumnSchema )
projects =
  TableSchema
    { tableName = "project"
    , tableSchema = Nothing
    , tableColumns = Project { projectId = "id" }
    }


data ProjectPart f =
  ProjectPart
    { projectPartProjectId :: Column f Int
    , projectPartPartId :: Column f Int
    }


-- TODO Generically derive.
instance HigherKinded ProjectPart where
  type ZipRecord ProjectPart f g c =
    ( CanZipLeaves ( C f Int ) ( C g Int ) c
    , ZipLeaves ( C f Int ) ( C g Int ) f g
    )

  zipRecord
    :: forall proxy c f g m
     . ( Applicative m, ZipRecord ProjectPart f g c )
    => proxy c
    -> (forall x. c x => C f x -> C f x -> m (C g x))
    -> ProjectPart f -> ProjectPart f -> m (ProjectPart g)
  zipRecord proxy f a b =
    ProjectPart
      <$> do toColumn @g @Int <$>
               zipLeaves proxy f (C @f @Int (projectPartProjectId a)) (C (projectPartProjectId b))
      <*> do toColumn @g @Int <$>
               zipLeaves proxy f (C @f @Int (projectPartPartId a)) (C (projectPartPartId b))


projectParts :: TableSchema ( ProjectPart ColumnSchema )
projectParts =
  TableSchema
    { tableName = "project_part"
    , tableSchema = Nothing
    , tableColumns = ProjectPart { projectPartPartId = "part_id"
                                 , projectPartProjectId = "project_id"
                                 }
    }


leftJoinTest :: MonadQuery m => m ( Expr m Int, MaybeTable ( ProjectPart ( Expr m ) ) )
leftJoinTest = do
  Part{ partId } <-
    each parts

  projectPart <-
    leftJoin ( each projectParts ) \ProjectPart{ projectPartPartId } ->
      projectPartPartId ==. partId

  return ( partId, projectPart )


data PartWithProject f =
  PartWithProject
    { part :: Part f
    , project :: Project f
    }


-- TODO Generically derive.
instance HigherKinded PartWithProject where
  type ZipRecord PartWithProject f g c =
    ( CanZipLeaves ( Part f ) ( Part g ) c
    , ZipLeaves ( Part f ) ( Part g ) f g
    , CanZipLeaves ( Project f ) ( Project g ) c
    , ZipLeaves ( Project f ) ( Project g ) f g
    )

  zipRecord
    :: forall proxy c f g m
     . ( Applicative m, ZipRecord PartWithProject f g c )
    => proxy c
    -> (forall x. c x => C f x -> C f x -> m (C g x))
    -> PartWithProject f -> PartWithProject f -> m (PartWithProject g)
  zipRecord proxy f a b =
    PartWithProject
      <$> zipLeaves proxy f (part a) (part b)
      <*> zipLeaves proxy f (project a) (project b)


partsWithProjects :: MonadQuery m => m ( PartWithProject ( Expr m ) )
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


nestedTableEq :: MonadQuery m => m ( Expr m Bool )
nestedTableEq = do
  l <- partsWithProjects
  r <- partsWithProjects
  return ( l ==. r )


select_partsWithProjects =
  select partsWithProjects
