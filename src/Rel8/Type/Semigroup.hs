{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Type.Semigroup
  ( DBSemigroup( (<>.))
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.Type.Equality ( (:~:)( Refl ) )
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
import Rel8.Expr.Array ( sappend )
import Rel8.Expr.Opaleye ( zipPrimExprsWith )
import Rel8.Kind.Blueprint ( blueprintRoundtripsViaDBType )
import Rel8.Kind.Emptiability ( KnownEmptiability, emptiabilitySing )
import Rel8.Kind.Nullability ( KnownNullability, nullabilitySing )
import Rel8.Type ( DBType, blueprintForDBType )
import Rel8.Type.Array ( Array )

-- text
import Data.Text ( Text )
import qualified Data.Text.Lazy as Lazy ( Text )

-- time
import Data.Time.Clock ( DiffTime, NominalDiffTime )


type DBSemigroup :: Type -> Constraint
class DBType a => DBSemigroup a where
  (<>.) :: Expr nullability a -> Expr nullability a -> Expr nullability a
  infixr 6 <>.


instance
  ( KnownEmptiability emptiability
  , KnownNullability nullability
  , DBType a
  ) => DBSemigroup (Array emptiability nullability a)
 where
  (<>.) = case blueprintForDBType @a of
    blueprint -> case blueprintRoundtripsViaDBType @a blueprint of
      Refl -> sappend emptiabilitySing nullabilitySing blueprint


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
