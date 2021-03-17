{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Expr.Function
  ( Function, function
  , nullaryFunction
  , binaryOperator
  , unsafeBinaryOperator
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Opaleye
  ( castExpr
  , fromPrimExpr, toPrimExpr, zipPrimExprsWith
  , unsafeZipPrimExprsWith
  )
import Rel8.Kind.Blueprint ( KnownBlueprint, FromDBType, ToDBType )
import Rel8.Type ( DBType )


type Function :: Type -> Type -> Constraint
class Function arg res where
  applyArgument :: ([Opaleye.PrimExpr] -> Opaleye.PrimExpr) -> arg -> res


instance
  ( iblueprint ~ FromDBType a
  , ToDBType iblueprint ~ a
  , KnownBlueprint iblueprint
  , oblueprint ~ FromDBType b
  , ToDBType oblueprint ~ b
  , KnownBlueprint oblueprint
  , arg ~ Expr nullability a
  , DBType b
  ) => Function arg (Expr nullability b) where
  applyArgument f a = castExpr $ fromPrimExpr $ f [toPrimExpr a]


instance
  ( blueprint ~ FromDBType a
  , ToDBType blueprint ~ a
  , KnownBlueprint blueprint
  , arg ~ Expr nullability a
  , Function args res
  ) => Function arg (args -> res) where
  applyArgument f a = applyArgument (f . (toPrimExpr a :))


function :: Function args result => String -> args -> result
function = applyArgument . Opaleye.FunExpr


nullaryFunction :: DBType a => String -> Expr nullability a
nullaryFunction name = castExpr $ Expr (Opaleye.FunExpr name [])


binaryOperator ::
  ( ablueprint ~ FromDBType a
  , a ~ ToDBType ablueprint
  , KnownBlueprint ablueprint
  , bblueprint ~ FromDBType b
  , b ~ ToDBType bblueprint
  , KnownBlueprint bblueprint
  , cblueprint ~ FromDBType c
  , c ~ ToDBType cblueprint
  , KnownBlueprint cblueprint
  , DBType c
  )
  => String
  -> Expr nullabilityA a -> Expr nullabilityB b -> Expr nullabilityC c
binaryOperator operator a b =
  castExpr $ zipPrimExprsWith (Opaleye.BinExpr (Opaleye.OpOther operator)) a b


unsafeBinaryOperator :: DBType c
  => String
  -> Expr nullabilityA a -> Expr nullabilityB b -> Expr nullabilityC c
unsafeBinaryOperator operator a b =
  castExpr $ unsafeZipPrimExprsWith (Opaleye.BinExpr (Opaleye.OpOther operator)) a b
