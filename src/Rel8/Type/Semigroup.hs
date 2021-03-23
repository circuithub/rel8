{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Type.Semigroup
  ( DBSemigroup( (<>.))
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import Prelude ()

-- bytestring
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as Lazy ( ByteString )

-- case-insensitive
import Data.CaseInsensitive ( CI )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( zipPrimExprsWith )
import Rel8.Schema.Nullability ( Sql )
import Rel8.Type ( DBType )

-- text
import Data.Text ( Text )
import qualified Data.Text.Lazy as Lazy ( Text )

-- time
import Data.Time.Clock ( DiffTime, NominalDiffTime )


type DBSemigroup :: Type -> Constraint
class DBType a => DBSemigroup a where
  (<>.) :: Expr a -> Expr a -> Expr a
  infixr 6 <>.


instance Sql DBType a => DBSemigroup [a] where
  (<>.) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:||))


instance Sql DBType a => DBSemigroup (NonEmpty a) where
  (<>.) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:||))


instance DBSemigroup DiffTime where
  (<>.) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:+))


instance DBSemigroup NominalDiffTime where
  (<>.) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:+))


instance DBSemigroup Text where
  (<>.) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:||))


instance DBSemigroup Lazy.Text where
  (<>.) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:||))


instance DBSemigroup (CI Text) where
  (<>.) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:||))


instance DBSemigroup (CI Lazy.Text) where
  (<>.) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:||))


instance DBSemigroup ByteString where
  (<>.) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:||))


instance DBSemigroup Lazy.ByteString where
  (<>.) = zipPrimExprsWith (Opaleye.BinExpr (Opaleye.:||))
