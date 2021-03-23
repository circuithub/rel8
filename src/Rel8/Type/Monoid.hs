{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}
{-# language StandaloneKindSignatures #-}
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
import Rel8.Expr.Serialize ( litExpr )
import Rel8.Schema.Nullability ( Nullabilizes, nullabilization )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Semigroup ( DBSemigroup )

-- text
import Data.Text ( Text )
import qualified Data.Text.Lazy as Lazy ( Text )

-- time
import Data.Time.Clock ( DiffTime, NominalDiffTime )


type DBMonoid :: Type -> Constraint
class DBSemigroup a => DBMonoid a where
  memptyExpr :: Expr a


instance (DBType db, Nullabilizes db a) => DBMonoid [a] where
  memptyExpr = sempty nullabilization typeInformation


instance DBMonoid DiffTime where
  memptyExpr = litExpr 0


instance DBMonoid NominalDiffTime where
  memptyExpr = litExpr 0


instance DBMonoid Text where
  memptyExpr = litExpr ""


instance DBMonoid Lazy.Text where
  memptyExpr = litExpr ""


instance DBMonoid (CI Text) where
  memptyExpr = litExpr ""


instance DBMonoid (CI Lazy.Text) where
  memptyExpr = litExpr ""


instance DBMonoid ByteString where
  memptyExpr = litExpr ""


instance DBMonoid Lazy.ByteString where
  memptyExpr = litExpr ""
