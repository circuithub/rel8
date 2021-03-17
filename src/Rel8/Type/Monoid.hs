{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Type.Monoid
  ( DBMonoid( memptyExpr )
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude hiding ( null )

-- bytestring
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as Lazy ( ByteString )

-- case-insensitive
import Data.CaseInsensitive ( CI )

-- rel8
import {-# SOURCE #-} Rel8.Expr ( Expr )
import Rel8.Expr.Array ( sempty )
import Rel8.Expr.Opaleye ( litPrimExpr )
import Rel8.Kind.Blueprint
  ( KnownBlueprint, blueprintSing
  , FromDBType, ToDBType
  )
import Rel8.Kind.Emptiability ( Emptiability( Emptiable ) )
import Rel8.Kind.Nullability
  ( KnownNullability, nullabilitySing
  )
import Rel8.Type ( DBType )
import Rel8.Type.Array ( Array )
import Rel8.Type.Semigroup ( DBSemigroup )

-- text
import Data.Text ( Text )
import qualified Data.Text.Lazy as Lazy ( Text )

-- time
import Data.Time.Clock ( DiffTime, NominalDiffTime )


type DBMonoid :: Type -> Constraint
class DBSemigroup a => DBMonoid a where
  memptyExpr :: Expr nullability a


instance
  ( emptiability ~ 'Emptiable
  , KnownNullability nullability
  , KnownBlueprint blueprint
  , blueprint ~ FromDBType a
  , a ~ ToDBType blueprint
  , DBType a
  ) => DBMonoid (Array emptiability nullability a)
 where
  memptyExpr = sempty nullabilitySing (blueprintSing @blueprint)


instance DBMonoid DiffTime where
  memptyExpr = litPrimExpr 0


instance DBMonoid NominalDiffTime where
  memptyExpr = litPrimExpr 0


instance DBMonoid Text where
  memptyExpr = litPrimExpr ""


instance DBMonoid Lazy.Text where
  memptyExpr = litPrimExpr ""


instance DBMonoid (CI Text) where
  memptyExpr = litPrimExpr ""


instance DBMonoid (CI Lazy.Text) where
  memptyExpr = litPrimExpr ""


instance DBMonoid ByteString where
  memptyExpr = litPrimExpr ""


instance DBMonoid Lazy.ByteString where
  memptyExpr = litPrimExpr ""
