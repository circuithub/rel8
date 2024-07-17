{-# language FlexibleContexts #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Opaleye
  ( castExpr, unsafeCastExpr
  , scastExpr, sunsafeCastExpr
  , unsafeLiteral
  , fromPrimExpr, toPrimExpr, mapPrimExpr, zipPrimExprsWith, traversePrimExpr
  , toColumn, fromColumn, traverseFieldP
  )
where

-- base
import Prelude

-- opaleye
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Expr ( Expr( Expr ) )
import Rel8.Schema.Null ( Unnullify, Sql )
import Rel8.Type ( DBType, typeInformation )
import Rel8.Type.Information ( TypeInformation(..) )
import Rel8.Type.Name (TypeName, showTypeName)

-- profunctors
import Data.Profunctor ( Profunctor, dimap )


castExpr :: Sql DBType a => Expr a -> Expr a
castExpr = scastExpr typeInformation


-- | Cast an expression to a different type. Corresponds to a @CAST()@ function
-- call.
unsafeCastExpr :: forall b a. Sql DBType b => Expr a -> Expr b
unsafeCastExpr = case typeInformation @(Unnullify b) of
  TypeInformation {typeName} -> sunsafeCastExpr typeName


scastExpr :: TypeInformation (Unnullify a) -> Expr a -> Expr a
scastExpr TypeInformation {typeName} = sunsafeCastExpr typeName


sunsafeCastExpr :: ()
  => TypeName -> Expr a -> Expr b
sunsafeCastExpr name =
  fromPrimExpr . Opaleye.CastExpr (showTypeName name) . toPrimExpr


-- | Unsafely construct an expression from literal SQL.
--
-- This is an escape hatch, and can be used if Rel8 can not adequately express
-- the query you need. If you find yourself using this function, please let us
-- know, as it may indicate that something is missing from Rel8!
unsafeLiteral :: String -> Expr a
unsafeLiteral = Expr . Opaleye.ConstExpr . Opaleye.OtherLit


fromPrimExpr :: Opaleye.PrimExpr -> Expr a
fromPrimExpr = Expr


toPrimExpr :: Expr a -> Opaleye.PrimExpr
toPrimExpr (Expr a) = a


mapPrimExpr :: (Opaleye.PrimExpr -> Opaleye.PrimExpr) -> Expr a -> Expr b
mapPrimExpr f = fromPrimExpr . f . toPrimExpr


zipPrimExprsWith :: ()
  => (Opaleye.PrimExpr -> Opaleye.PrimExpr -> Opaleye.PrimExpr)
  -> Expr a -> Expr b -> Expr c
zipPrimExprsWith f a b = fromPrimExpr (f (toPrimExpr a) (toPrimExpr b))


traversePrimExpr :: Functor f
  => (Opaleye.PrimExpr -> f Opaleye.PrimExpr) -> Expr a -> f (Expr b)
traversePrimExpr f = fmap fromPrimExpr . f . toPrimExpr


traverseFieldP :: Profunctor p
  => p (Opaleye.Field_ n x) (Opaleye.Field_ m y)
  -> p (Expr a) (Expr b)
traverseFieldP =  dimap (toColumn . toPrimExpr) (fromPrimExpr . fromColumn)


toColumn :: Opaleye.PrimExpr -> Opaleye.Field_ n b
toColumn = Opaleye.Column


fromColumn :: Opaleye.Field_ n b -> Opaleye.PrimExpr
fromColumn (Opaleye.Column a) = a
