{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RoleAnnotations #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Expr where

import Data.Kind
import Rel8.Column
import Rel8.Table
import Rel8.ZipLeaves
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye


-- | Typed SQL expressions
data Expr ( m :: Type -> Type ) a =
  Expr { toPrimExpr :: Opaleye.PrimExpr }

type role Expr representational nominal


instance Table ( Expr m a ) where
  type ExprIn ( Expr m a ) = Expr m


class DBType a where
  lit :: a -> Expr m a


instance DBType Bool where
  lit =
    Expr . Opaleye.ConstExpr . Opaleye.BoolLit


instance a ~ b => ZipLeaves ( Expr m a ) ( Expr n b ) ( Expr m ) ( Expr n ) where
  type CanZipLeaves ( Expr m a ) ( Expr n b ) c =
    c a

  zipLeaves _ f e1 e2 =
    toColumn <$> f ( C e1 ) ( C e2 )
