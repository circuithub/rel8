{-# language StandaloneKindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
module Rel8.Type.DBIsString ( DBIsString(..) ) where

import {-# source #-} Rel8.Expr ( Expr )
import Rel8.Expr.Opaleye ( litPrimExpr )
import Data.Kind ( Type, Constraint )
import Data.String (IsString, fromString)
import Rel8.Type (DBType)

type DBIsString :: Type -> Constraint
class DBType a => DBIsString a where
  fromStringExpr :: String -> Expr a

  default fromStringExpr :: IsString a => String -> Expr a
  fromStringExpr = litPrimExpr . fromString
