{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RoleAnnotations #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Expr where

import Data.Coerce
import Data.Kind
import Rel8.Column
import Rel8.Table
import Rel8.ZipLeaves
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye


-- | Typed SQL expressions
data Expr ( m :: Type -> Type ) ( a :: Type ) =
  Expr { toPrimExpr :: Opaleye.PrimExpr }


type role Expr representational representational


instance Table ( Expr m a ) where
  type ExprIn ( Expr m a ) = Expr m


-- | The class of all Haskell types that can be represented as expressiosn
-- in a database. There should be an instance of @DBType@ for all column
-- types in your database schema (e.g., @int@, @timestamptz@, etc).
--
-- Rel8 comes with stock instances for all default types in PostgreSQL.
class DBType ( a :: Type ) where
  -- | Lift a Haskell value into a literal SQL expression.
  lit :: a -> Expr m a


instance DBType Bool where
  lit =
    Expr . Opaleye.ConstExpr . Opaleye.BoolLit


instance DBType Int where
  lit =
    Expr . Opaleye.ConstExpr . Opaleye.IntegerLit . fromIntegral


instance a ~ b => ZipLeaves ( Expr m a ) ( Expr n b ) ( Expr m ) ( Expr n ) where
  type CanZipLeaves ( Expr m a ) ( Expr n b ) c =
    c a

  zipLeaves _ f e1 e2 =
    toColumn <$> f ( C e1 ) ( C e2 )


-- | The SQL @AND@ operator.
(&&.) :: Expr m Bool -> Expr m Bool -> Expr m Bool
(&&.) ( Expr a ) ( Expr b ) =
    Expr ( Opaleye.BinExpr (Opaleye.:&&) a b )


-- | The SQL @OR@ operator.
(||.) :: Expr m Bool -> Expr m Bool -> Expr m Bool
(||.) ( Expr a ) ( Expr b ) =
    Expr ( Opaleye.BinExpr (Opaleye.:||) a b )


coerceExpr :: Coercible a b => Expr m a -> Expr m b
coerceExpr = unsafeCoerceExpr


unsafeCoerceExpr :: Expr m a -> Expr m b
unsafeCoerceExpr ( Expr x ) = Expr x
