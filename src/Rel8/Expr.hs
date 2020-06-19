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
  , dbBinOp
  , unsafeCoerceExpr
  , null_
  , isNull
  , liftNull
  , traversePrimExpr
  , ifThenElse_
  ) where

import Data.Coerce
import Data.Foldable ( foldl' )
import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import Data.String
import GHC.Generics
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Column
import Rel8.DBType
import Rel8.Table
import Rel8.Unconstrained


-- | Typed SQL expressions
newtype Expr ( a :: Type ) =
  Expr { toPrimExpr :: Opaleye.PrimExpr }


type role Expr representational


instance ( IsString a, DBType a ) => IsString ( Expr a ) where
  fromString =
    lit . fromString


-- | The SQL @AND@ operator.
(&&.) :: Expr Bool -> Expr Bool -> Expr Bool
(&&.) ( Expr a ) ( Expr b ) =
    Expr ( Opaleye.BinExpr Opaleye.OpAnd a b )


-- | The SQL @OR@ operator.
(||.) :: Expr Bool -> Expr Bool -> Expr Bool
(||.) ( Expr a ) ( Expr b ) =
    Expr ( Opaleye.BinExpr Opaleye.OpOr a b )


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


unsafeCoerceExpr :: Expr a -> Expr b
unsafeCoerceExpr ( Expr x ) = Expr x


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
  const ( Expr ( Opaleye.FunExpr name [] ) ) ( lit @a undefined )


-- | Any 'Expr' can be seen as a 'Table' with only one column.
instance Table (Expr a) where
  type Context (Expr a) = Expr
  type Structure (Expr a) = HIdentity a
  toStructure = HIdentity
  fromStructure = unHIdentity


binExpr :: Opaleye.BinOp -> Expr a -> Expr a -> Expr b
binExpr op ( Expr a ) ( Expr b ) =
    Expr ( Opaleye.BinExpr op a b )


column :: String -> Expr a
column columnName =
  Expr ( Opaleye.BaseTableAttrExpr columnName )


fromPrimExpr :: Opaleye.PrimExpr -> Expr a
fromPrimExpr =
  Expr


retype :: Expr a -> Expr b
retype =
  fromPrimExpr . toPrimExpr


null_ :: DBType b => Expr b -> ( Expr a -> Expr b ) -> Expr ( Maybe a ) -> Expr b
null_ whenNull f a =
 ifThenElse_ ( isNull a ) whenNull ( f ( retype a ) )


ifThenElse_ :: ExprTable a => Expr Bool -> a -> a -> a
ifThenElse_ bool whenTrue whenFalse =
  case_ [ ( bool, whenTrue ) ] whenFalse


case_ :: forall a. ExprTable a => [ ( Expr Bool, a ) ] -> a -> a
case_ alts def =
  fromStructure $ runIdentity $ htabulate @(Structure a) (Proxy @Unconstrained) \x ->
    pure $ MkC $ fromPrimExpr $
    Opaleye.CaseExpr
        [ ( toPrimExpr bool, toPrimExpr $ toColumn $ hfield (toStructure alt) x ) | ( bool, alt ) <- alts ]
        ( toPrimExpr $ toColumn $ hfield (toStructure def) x )


isNull :: Expr ( Maybe a ) -> Expr Bool
isNull =
  fromPrimExpr . Opaleye.UnExpr Opaleye.OpIsNull . toPrimExpr


traversePrimExpr
  :: Applicative f
  => ( Opaleye.PrimExpr -> f Opaleye.PrimExpr ) -> Expr a -> f ( Expr a )
traversePrimExpr f =
  fmap fromPrimExpr . f . toPrimExpr


liftNull :: Expr a -> Expr ( Maybe a )
liftNull =
  retype


and_ :: Foldable f => f ( Expr Bool ) -> Expr Bool
and_ =
  foldl' (&&.) ( lit True )


or_ :: Foldable f => f ( Expr Bool ) -> Expr Bool
or_ =
  foldl' (||.) ( lit False )


class (HConstrainTable (Structure a) Expr Unconstrained, Table a, Context a ~ Expr, HConstrainTable (Structure a) Expr DBType) => ExprTable a
instance (HConstrainTable (Structure a) Expr Unconstrained, Table a, Context a ~ Expr, HConstrainTable (Structure a) Expr DBType) => ExprTable a


-- | Lift a Haskell value into a literal SQL expression.
lit :: DBType a => a -> Expr a
lit = Expr . Opaleye.CastExpr typeName . encode
  where
    DatabaseType{ encode, typeName } = typeInformation


-- | Corresponds to the @ILIKE@ operator.
dbBinOp :: String -> Expr a -> Expr b -> Expr c
dbBinOp op (Expr a) (Expr b) =
  Expr $ Opaleye.BinExpr (Opaleye.OpOther op) a b
