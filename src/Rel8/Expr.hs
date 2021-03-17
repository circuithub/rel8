{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}

module Rel8.Expr ( Expr(..) ) where

-- base
import Data.Kind ( Type )
import Rel8.Type.DBMonoid ( DBMonoid(..) )
import Rel8.Type.DBSemigroup ( DBSemigroup(..) )
import Rel8.Type.DBNum ( DBFractional(..), DBNum(..) )
import Rel8.Type.DBIsString ( DBIsString(..) )

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Data.String (IsString(..))


-- | Typed SQL expressions
type Expr :: Type -> Type
newtype Expr a = Expr { toPrimExpr :: Opaleye.PrimExpr }


instance DBSemigroup a => Semigroup (Expr a) where
  (<>) = (<>.)


instance DBMonoid a => Monoid (Expr a) where
  mempty = memptyExpr


instance DBNum a => Num (Expr a) where
  (+) = (+.)
  (-) = (-.)
  (*) = (*.)
  abs = absExpr
  signum = signumExpr
  fromInteger = fromIntegerExpr


instance DBIsString a => IsString (Expr a) where
  fromString = fromStringExpr


instance DBFractional a => Fractional (Expr a) where
  fromRational = fromRationalExpr
  recip = recipExpr

