{-# language DataKinds #-}
{-# language GADTs #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

{-# options -Wno-orphans #-}

module Rel8.Expr.Instances ( Column( ExprColumn, fromExprColumn ) ) where

-- base
import Data.Kind ( Type )
import Data.String ( IsString( fromString ) )

-- rel8
import Rel8.Context ( Context( Column ), Meta( Meta ) )
import Rel8.DBType ( DBType )
import Rel8.DBType.DBMonoid ( DBMonoid( memptyExpr ) )
import Rel8.DBType.DBNum ( DBFractional( (/.), fromRationalExpr ), DBNum( (+.), (-.), (*.), absExpr, signumExpr, fromIntegerExpr, negateExpr ) )
import Rel8.DBType.DBSemigroup ( DBSemigroup( (<>.) ) )
import Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( litExpr )


instance Context Expr where
  data Column Expr :: Meta -> Type where
    ExprColumn :: { fromExprColumn :: Expr a } -> Column Expr ('Meta defaulting a)


-- | It is assumed that any Haskell types that have a 'Num' instance also have
-- the corresponding operations in the database. Hence, Num a => Num (Expr a).
-- *However*, if this is not the case, you should `newtype` the Haskell type
-- and avoid providing a 'Num' instance, or you may write be able to write
-- ill-typed queries!
instance DBNum a => Num (Expr a) where
  (+) = (+.)
  (-) = (-.)
  (*) = (*.)
  abs = absExpr
  signum = signumExpr
  fromInteger = fromIntegerExpr
  negate = negateExpr


instance DBFractional a => Fractional (Expr a) where
  (/) = (/.)
  fromRational = fromRationalExpr


instance (IsString a, DBType a) => IsString (Expr a) where
  fromString = litExpr . fromString


instance DBSemigroup a => Semigroup (Expr a) where
  (<>) = (<>.)


instance DBMonoid a => Monoid (Expr a) where
  mempty = memptyExpr
