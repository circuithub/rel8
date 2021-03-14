{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DerivingStrategies #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
{-# language RoleAnnotations #-}
{-# language StandaloneKindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module Rel8.Expr
  ( Expr(..)
  , DBSemigroup( (<>.))
  , DBMonoid( memptyExpr )
  , castExpr
  , null
  , seminullify, unsafeLiftOpSeminullable, unsafeUnnullify
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.String ( IsString, fromString )
import Prelude hiding ( null )

-- bytestring
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as Lazy ( ByteString )

-- case-insensitive
import Data.CaseInsensitive ( CI )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Kind.Nullability ( Nullability( NonNullable, Nullable ) )
import Rel8.Type ( DBType, cast, encode, typeInformation )
import Rel8.Type.Num ( DBFractional, DBNum )

-- text
import Data.Text ( Text )
import qualified Data.Text.Lazy as Lazy ( Text )

-- time
import Data.Time.Clock ( DiffTime, NominalDiffTime )


type role Expr representational representational
type Expr :: Nullability -> Type -> Type
newtype Expr nullability a = Expr
  { toPrimExpr :: Opaleye.PrimExpr
  }
  deriving stock Show


instance DBSemigroup a => Semigroup (Expr nullability a) where
  (<>) = unsafeLiftOpSeminullable (<>.)


instance DBMonoid a => Monoid (Expr nullability a) where
  mempty = seminullify memptyExpr


instance (IsString a, DBType a) => IsString (Expr nullability a) where
  fromString = seminullify . litExpr . fromString


instance DBNum a => Num (Expr nullability a) where
  Expr a + Expr b = Expr (Opaleye.BinExpr (Opaleye.:+) a b)
  Expr a * Expr b = Expr (Opaleye.BinExpr (Opaleye.:*) a b)

  abs (Expr a) = Expr (Opaleye.UnExpr Opaleye.OpAbs a)
  negate (Expr a) = Expr (Opaleye.UnExpr Opaleye.OpNegate a)

  signum (Expr a) =
    castExpr (Expr (Opaleye.UnExpr (Opaleye.UnOpOther "SIGN") a))

  fromInteger = castExpr . Expr . Opaleye.ConstExpr . Opaleye.IntegerLit


instance DBFractional a => Fractional (Expr nullability a) where
  Expr a / Expr b = Expr (Opaleye.BinExpr (Opaleye.:/) a b)

  fromRational =
    castExpr . Expr . Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac


castExpr :: forall a nullability. DBType a
  => Expr nullability a -> Expr nullability a
castExpr (Expr a) = Expr (cast info a)
  where
    info = typeInformation @a


litExpr :: DBType a => a -> Expr 'NonNullable a
litExpr = castExpr . Expr . encode info
  where
    info = typeInformation


null :: DBType a => Expr 'Nullable a
null = castExpr $ Expr $ Opaleye.ConstExpr Opaleye.NullLit


seminullify :: Expr 'NonNullable a -> Expr nullability a
seminullify (Expr a) = Expr a


unsafeLiftOpSeminullable :: ()
  => (Expr 'NonNullable a -> Expr 'NonNullable b -> Expr 'NonNullable c)
  -> Expr nullability a -> Expr nullability b -> Expr nullability c
unsafeLiftOpSeminullable f ma mb =
  seminullify (f (unsafeUnnullify ma) (unsafeUnnullify mb))


unsafeUnnullify :: Expr nullability a -> Expr 'NonNullable a
unsafeUnnullify (Expr a) = Expr a


type DBSemigroup :: Type -> Constraint
class DBType a => DBSemigroup a where
  (<>.) :: Expr 'NonNullable a -> Expr 'NonNullable a -> Expr 'NonNullable a


instance DBSemigroup DiffTime where
  Expr a <>. Expr b = Expr (Opaleye.BinExpr (Opaleye.:+) a b)


instance DBSemigroup NominalDiffTime where
  Expr a <>. Expr b = Expr (Opaleye.BinExpr (Opaleye.:+) a b)


instance DBSemigroup Text where
  Expr a <>. Expr b = Expr (Opaleye.BinExpr (Opaleye.:||) a b)


instance DBSemigroup Lazy.Text where
  Expr a <>. Expr b = Expr (Opaleye.BinExpr (Opaleye.:||) a b)


instance DBSemigroup (CI Text) where
  Expr a <>. Expr b = Expr (Opaleye.BinExpr (Opaleye.:||) a b)


instance DBSemigroup (CI Lazy.Text) where
  Expr a <>. Expr b = Expr (Opaleye.BinExpr (Opaleye.:||) a b)


instance DBSemigroup ByteString where
  Expr a <>. Expr b = Expr (Opaleye.BinExpr (Opaleye.:||) a b)


instance DBSemigroup Lazy.ByteString where
  Expr a <>. Expr b = Expr (Opaleye.BinExpr (Opaleye.:||) a b)


type DBMonoid :: Type -> Constraint
class DBSemigroup a => DBMonoid a where
  memptyExpr :: Expr 'NonNullable a


instance DBMonoid DiffTime where
  memptyExpr = litExpr 0


instance DBMonoid NominalDiffTime where
  memptyExpr = litExpr 0


instance DBMonoid Text where
  memptyExpr = ""


instance DBMonoid Lazy.Text where
  memptyExpr = ""


instance DBMonoid (CI Text) where
  memptyExpr = ""


instance DBMonoid (CI Lazy.Text) where
  memptyExpr = ""


instance DBMonoid ByteString where
  memptyExpr = ""


instance DBMonoid Lazy.ByteString where
  memptyExpr = ""
