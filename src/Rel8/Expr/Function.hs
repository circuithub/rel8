{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Expr.Function
  ( Arguments, function
  , binaryOperator
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Expr (Expr)
import Rel8.Expr.Opaleye
  ( castExpr
  , fromPrimExpr, toPrimExpr, zipPrimExprsWith
  )
import Rel8.Schema.HTable (hfoldMap)
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.QualifiedName (QualifiedName, ppQualifiedName)
import Rel8.Table (Table, toColumns)
import Rel8.Type ( DBType )


-- | This type class is basically @'Table' 'Expr'@, where each column of the
-- 'Table' is an argument to the function, but it also has an additional
-- instance for @()@ for calling functions with no arguments.
type Arguments :: Type -> Constraint
class Arguments a where
  arguments :: a -> [Opaleye.PrimExpr]


instance Table Expr a => Arguments a where
  arguments = hfoldMap (pure . toPrimExpr) . toColumns


instance {-# OVERLAPS #-} Arguments () where
  arguments _ = []


-- | @'function' name arguments@ runs the PostgreSQL function @name@ with
-- the arguments @arguments@ returning an @'Expr' a@.
function :: (Arguments arguments, Sql DBType a)
  => QualifiedName -> arguments -> Expr a
function qualified = castExpr . fromPrimExpr . Opaleye.FunExpr name . arguments
  where
    name = show (ppQualifiedName qualified)


-- | Construct an expression by applying an infix binary operator to two
-- operands.
binaryOperator :: Sql DBType c => String -> Expr a -> Expr b -> Expr c
binaryOperator operator a b =
  castExpr $ zipPrimExprsWith (Opaleye.BinExpr (Opaleye.OpOther operator)) a b
