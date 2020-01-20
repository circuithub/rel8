{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Expr
  ( DBType(..)
  , (&&.)
  , (||.)
  , Expr
  , Function
  , applyArgument
  , binExpr
  , coerceExpr
  , column
  , dbFunction
  , demote
  , fromPrimExpr
  , not_
  , nullaryFunction
  , promote
  , retype
  , toPrimExpr
  , unsafeCoerceExpr
  , null_
  , traversePrimExpr
  ) where

import Data.Coerce
import Data.Int
import Data.Kind
import Data.Proxy
import Data.String
import Data.Text ( Text, unpack )
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Column
import Rel8.Nest
import Rel8.Table


-- | Typed SQL expressions
newtype Expr ( m :: Type -> Type ) ( a :: Type ) =
  Expr { toPrimExpr :: Opaleye.PrimExpr }


type role Expr representational representational


instance ( IsString a, DBType a ) => IsString ( Expr m a ) where
  fromString =
    lit . fromString


{-| Haskell types that can be represented as expressiosn in a database. There
should be an instance of @DBType@ for all column types in your database
schema (e.g., @int@, @timestamptz@, etc).

Rel8 comes with stock instances for all default types in PostgreSQL.

[ @newtype@ing @DBType@s ]

Generalized newtype deriving can be used when you want use a @newtype@ around a
database type for clarity and accuracy in your Haskell code. A common example is
to @newtype@ row id types:

@
newtype UserId = UserId { toInt32 :: Int32 }
  deriving ( DBType )
@

You can now write queries using @UserId@ instead of @Int32@, which may help
avoid making bad joins. However, when SQL is generated, it will be as if you
just used integers (the type distinction does not impact query generation).

[ Using @Show@ with @DBType@ ]

@DBType@ also comes with a default instance using @Show@. This can be useful if
you have a small enumeration type that you need to store in your database, and
you're happy to just encode it as a string:

@
data Color = Red | Green | Blue | Purple | Gold
  deriving ( Show, DBType )
@

-}

class DBType ( a :: Type ) where
  -- | Lift a Haskell value into a literal SQL expression.
  lit :: a -> Expr m a

  default lit :: Show a => a -> Expr m a
  lit = unsafeCoerceExpr . lit . show


-- | Corresponds to the @bool@ PostgreSQL type.
instance DBType Bool where
  lit =
    Expr . Opaleye.ConstExpr . Opaleye.BoolLit


-- | Corresponds to the @int4@ PostgreSQL type.
instance DBType Int32 where
  lit =
    Expr . Opaleye.ConstExpr . Opaleye.IntegerLit . fromIntegral


-- | Corresponds to the @int8@ PostgreSQL type.
instance DBType Int64 where
  lit =
    Expr . Opaleye.ConstExpr . Opaleye.IntegerLit . fromIntegral


-- | Corresponds to the @text@ PostgreSQL type.
instance DBType Text where
  lit =
    unsafeCoerceExpr . lit . unpack


-- | Corresponds to the @text@ PostgreSQL type.
instance DBType String where
  lit =
    Expr . Opaleye.ConstExpr . Opaleye.StringLit


-- | Extends any @DBType@ with the value @null@. Note that you cannot "stack"
-- @Maybe@s, as SQL doesn't distinguish @Just Nothing@ from @Nothing@.
instance DBTypeMaybe ( IsMaybe a ) a => DBType ( Maybe a ) where
  lit =
    maybe
      ( Expr ( Opaleye.ConstExpr Opaleye.NullLit ) )
      ( retype . maybeLit ( Proxy @( IsMaybe a ) ) )


type family IsMaybe ( a :: Type ) :: Bool where
  IsMaybe ( Maybe a ) = 'True
  IsMaybe a = 'False


class DBTypeMaybe ( isMaybe :: Bool ) a where
  maybeLit :: proxy isMaybe -> a -> Expr m a


instance DBType a => DBTypeMaybe 'False a where
  maybeLit _ = lit


-- | The SQL @AND@ operator.
(&&.) :: Expr m Bool -> Expr m Bool -> Expr m Bool
(&&.) ( Expr a ) ( Expr b ) =
    Expr ( Opaleye.BinExpr (Opaleye.:&&) a b )


-- | The SQL @OR@ operator.
(||.) :: Expr m Bool -> Expr m Bool -> Expr m Bool
(||.) ( Expr a ) ( Expr b ) =
    Expr ( Opaleye.BinExpr (Opaleye.:||) a b )


-- | The SQL @NOT@ operator.
not_ :: Expr m Bool -> Expr m Bool
not_ ( Expr a ) =
  Expr ( Opaleye.UnExpr Opaleye.OpNot a )


-- | Use Haskell's 'Coercible' type class to witness that two types
-- are actually compatible in the SQL expressions they produce.
coerceExpr :: forall b a m. Coercible a b => Expr m a -> Expr m b
coerceExpr e =
  const
    ( unsafeCoerceExpr e )
    ( coerce @a @b undefined )


unsafeCoerceExpr :: Expr m a -> Expr m b
unsafeCoerceExpr ( Expr x ) = Expr x


-- | The @Function@ type class is an implementation detail that allows
-- @dbFunction@ to be polymorphic in the number of arguments it consumes.
class Function arg res where
  -- | Build a function of multiple arguments.
  applyArgument :: ( [ Opaleye.PrimExpr ] -> Opaleye.PrimExpr ) -> arg -> res


instance arg ~ Expr m a => Function arg ( Expr m res ) where
  applyArgument mkExpr ( Expr a ) =
    Expr ( mkExpr [ a ] )


instance ( arg ~ Expr m a, Function args res ) => Function arg ( args -> res ) where
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
nullaryFunction :: DBType a => String -> Expr m a
nullaryFunction = nullaryFunction_forAll


nullaryFunction_forAll :: forall a m. DBType a => String -> Expr m a
nullaryFunction_forAll name =
  const ( Expr ( Opaleye.FunExpr name [] ) ) ( lit @a undefined )


promote :: Expr m a -> Expr ( Nest m ) a
promote ( Expr x ) =
  Expr x


demote :: Expr ( Nest m ) a -> Expr m a
demote ( Expr x ) =
  Expr x


data ExprField ( m :: * -> * ) a x where
  ExprField :: ExprField m a a


-- | Any 'Expr' can be seen as a 'Table' with only one column.
instance Table ( Expr m a ) where
  type Context ( Expr m a ) =
    Expr m

  type ConstrainTable ( Expr m a ) c =
    c a

  type Field ( Expr m a ) =
    ExprField m a

  field expr ExprField =
    MkC expr

  tabulateMCP _ f =
    toColumn <$> f ExprField


instance ( a ~ b, Expr m ~ m', Expr n ~ n' ) => Compatible ( Expr m a ) m' ( Expr n b ) n' where
  transferField ExprField =
    ExprField


binExpr :: Opaleye.BinOp -> Expr m a -> Expr m a -> Expr m b
binExpr op ( Expr a ) ( Expr b ) =
    Expr ( Opaleye.BinExpr op a b )


column :: String -> Expr m a
column columnName =
  Expr ( Opaleye.BaseTableAttrExpr columnName )


fromPrimExpr :: Opaleye.PrimExpr -> Expr m a
fromPrimExpr =
  Expr


retype :: Expr m a -> Expr m b
retype =
  fromPrimExpr . toPrimExpr


null_ :: Expr m b -> ( Expr m a -> Expr m b ) -> Expr m ( Maybe a ) -> Expr m b
null_ whenNull f a =
 ifThenElse_ ( isNull a ) whenNull ( f ( retype a ) )


ifThenElse_ :: Expr m Bool -> Expr m a -> Expr m a -> Expr m a
ifThenElse_ bool whenTrue whenFalse =
  case_ [ ( bool, whenTrue ) ] whenFalse


case_
  :: [ ( Expr m Bool, Expr m a ) ]
  -> Expr m a
  -> Expr m a
case_ alts def =
  fromPrimExpr
    ( Opaleye.CaseExpr
        [ ( toPrimExpr bool, toPrimExpr alt ) | ( bool, alt ) <- alts ]
        ( toPrimExpr def )
    )


isNull :: Expr m ( Maybe a ) -> Expr m Bool
isNull =
  fromPrimExpr . Opaleye.UnExpr Opaleye.OpIsNull . toPrimExpr


traversePrimExpr
  :: Applicative f
  => ( Opaleye.PrimExpr -> f Opaleye.PrimExpr ) -> Expr m a -> f ( Expr m a )
traversePrimExpr f =
  fmap fromPrimExpr . f . toPrimExpr
