{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Expr
  ( DBType(..)
  , DatabaseType(..)
  , parseDatabaseType
  , lit
  , (&&.)
  , (||.)
  , Expr
  , ExprTable
  , Function
  , and_
  , or_
  , applyArgument
  , binExpr
  , coerceExpr
  , column
  , dbFunction
  , fromPrimExpr
  , not_
  , nullaryFunction
  , retype
  , toPrimExpr
  , dbShow
  , unsafeCastExpr
  , dbBinOp
  , unsafeCoerceExpr
  , ilike
  , null_
  , isNull
  , liftNull
  , traversePrimExpr
  , ifThenElse_
  , DBMin
  , DBMax
  , dbNow
  , default_
  ) where

import Data.Coerce
import Data.Foldable ( foldl' )
import Data.Text ( Text )
import Data.Time ( UTCTime )
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Core


-- | The SQL @AND@ operator.
infixr 3 &&.
(&&.) :: Expr Bool -> Expr Bool -> Expr Bool
(&&.) ( Expr a ) ( Expr b ) =
    Expr ( Opaleye.BinExpr Opaleye.OpAnd a b )


-- | The SQL @NOT@ operator.
not_ :: Expr Bool -> Expr Bool
not_ ( Expr a ) =
  Expr ( Opaleye.UnExpr Opaleye.OpNot a )


-- | Use Haskell's 'Coercible' type class to witness that two types
-- are actually compatible in the SQL expressions they produce.
coerceExpr :: forall b a. Coercible a b => Expr a -> Expr b
coerceExpr e =
  const
    ( unsafeCoerceExpr e )
    ( coerce @a @b undefined )


-- | The @Function@ type class is an implementation detail that allows
-- @dbFunction@ to be polymorphic in the number of arguments it consumes.
class Function arg res where
  -- | Build a function of multiple arguments.
  applyArgument :: ( [ Opaleye.PrimExpr ] -> Opaleye.PrimExpr ) -> arg -> res


instance arg ~ Expr a => Function arg ( Expr res ) where
  applyArgument mkExpr ( Expr a ) =
    Expr ( mkExpr [ a ] )


instance ( arg ~ Expr a, Function args res ) => Function arg ( args -> res ) where
  applyArgument f ( Expr a ) =
    applyArgument ( f . ( a : ) )


{-| Construct an n-ary function that produces an 'Expr' that when called runs a
SQL function.

For example, if we have a SQL function @foo(x, y, z)@, we can represent this
in Rel8 with:

@
foo :: Expr m Int32 -> Expr m Int32 -> Expr m Bool -> Expr m Text
foo = dbFunction "foo"
@

-}
dbFunction :: Function args result => String -> args -> result
dbFunction =
  applyArgument . Opaleye.FunExpr


{-| Construct a function call for functions with no arguments.

As an example, we can call the database function @now()@ by using
@nullaryFunction@:

@
now :: Expr m UTCTime
now = nullaryFunction "now"
@

-}
nullaryFunction :: DBType a => String -> Expr a
nullaryFunction = nullaryFunction_forAll


nullaryFunction_forAll :: forall a. DBType a => String -> Expr a
nullaryFunction_forAll name =
  const (Expr ( Opaleye.FunExpr name [] )) (lit (undefined :: a))


binExpr :: Opaleye.BinOp -> Expr a -> Expr a -> Expr b
binExpr op ( Expr a ) ( Expr b ) =
    Expr ( Opaleye.BinExpr op a b )


column :: String -> Expr a
column columnName =
  Expr ( Opaleye.BaseTableAttrExpr columnName )


traversePrimExpr
  :: Applicative f
  => ( Opaleye.PrimExpr -> f Opaleye.PrimExpr ) -> Expr a -> f ( Expr a )
traversePrimExpr f =
  fmap fromPrimExpr . f . toPrimExpr


and_ :: Foldable f => f ( Expr Bool ) -> Expr Bool
and_ =
  foldl' (&&.) ( lit True )


or_ :: Foldable f => f ( Expr Bool ) -> Expr Bool
or_ =
  foldl' (||.) ( lit False )


-- | Corresponds to the @ILIKE@ operator.
dbBinOp :: String -> Expr a -> Expr b -> Expr c
dbBinOp op (Expr a) (Expr b) =
  Expr $ Opaleye.BinExpr (Opaleye.OpOther op) a b


class DBMin a
class DBMax a


ilike :: Expr Text -> Expr Text -> Expr Bool
Expr a `ilike` Expr b =
  Expr $ Opaleye.BinExpr (Opaleye.OpOther "ILIKE") a b


dbShow :: forall a. DBType a => Expr a -> Expr Text
dbShow = const (unsafeCastExpr "text") (lit (undefined :: a))


-- | Corresponds to the @now()@ function.
dbNow :: Expr UTCTime
dbNow = nullaryFunction "now"


default_ :: Expr a
default_ = Expr $ Opaleye.ConstExpr Opaleye.DefaultLit
