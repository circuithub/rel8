{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RecordWildCards #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Expr.Function
  ( Arguments
  , function
  , primFunction
  , binaryOperator
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- pretty
import Text.PrettyPrint (parens, text)

-- rel8
import {-# SOURCE #-} Rel8.Expr (Expr)
import Rel8.Expr.Opaleye
  ( castExpr
  , fromPrimExpr, toPrimExpr, zipPrimExprsWith
  )
import Rel8.Schema.Escape (escape)
import Rel8.Schema.HTable (hfoldMap)
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.QualifiedName (QualifiedName (..), showQualifiedName)
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
function qualified = castExpr . fromPrimExpr . primFunction qualified


primFunction :: Arguments arguments
  => QualifiedName -> arguments -> Opaleye.PrimExpr
primFunction qualified = Opaleye.FunExpr name . arguments
  where
    name = showQualifiedName qualified


-- | Construct an expression by applying an infix binary operator to two
-- operands.
binaryOperator :: Sql DBType c => QualifiedName -> Expr a -> Expr b -> Expr c
binaryOperator operator a b =
  castExpr $ zipPrimExprsWith (Opaleye.BinExpr (Opaleye.OpOther name)) a b
  where
    name = showQualifiedOperator operator


showQualifiedOperator :: QualifiedName -> String
showQualifiedOperator QualifiedName {schema = mschema, ..} = case mschema of
  Nothing -> name
  Just schema ->
    show $ text "OPERATOR" <> parens (escape schema <> text "." <> text name)
