{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
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
  , unsafeCoerceExpr
  , null_
  , isNull
  , liftNull
  , traversePrimExpr
  , litTable
  , ifThenElse_
  ) where

import Data.Profunctor ( Profunctor(..), dimap )
import Database.PostgreSQL.Simple.FromField ( FieldParser, fromField, optionalField )
import Data.Coerce
import Data.Proxy
import Data.Foldable ( foldl' )
import Data.Functor.Identity
import Data.Int
import Data.Kind
import Data.String
import Data.Text ( Text, unpack )
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Column
import Rel8.Table


-- | Typed SQL expressions
newtype Expr ( a :: Type ) =
  Expr { toPrimExpr :: Opaleye.PrimExpr }


type role Expr representational


instance ( IsString a, DBType a ) => IsString ( Expr a ) where
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
  typeInformation :: DatabaseType a a


-- | Lift a Haskell value into a literal SQL expression.
lit :: DBType a => a -> Expr a
lit = Expr . Opaleye.CastExpr typeName . encode
  where
    DatabaseType{ encode, typeName } = typeInformation


data DatabaseType a b =
  DatabaseType
    { encode :: a -> Opaleye.PrimExpr
    , decode :: FieldParser b
    , typeName :: String
    }


instance Profunctor DatabaseType where
  dimap f g DatabaseType{ encode, decode, typeName } = DatabaseType
    { encode = encode . f
    , decode = \x y -> fmap g $ decode x y
    , typeName
    }


litTable
  :: ( ConstrainTable ( MapTable Lit a ) DBType
     , Recontextualise a Lit
     , Context ( MapTable Lit a ) ~ Expr
     , Context a ~ Identity
     )
  => a -> MapTable Lit a
litTable =
  mapTableC @DBType @Lit ( mapCC @DBType lit )


-- | Corresponds to the @bool@ PostgreSQL type.
instance DBType Bool where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.BoolLit
    , decode = fromField
    , typeName = "bool"
    }


-- | Corresponds to the @int4@ PostgreSQL type.
instance DBType Int32 where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . fromIntegral
    , decode = fromField
    , typeName = "int4"
    }


-- | Corresponds to the @int8@ PostgreSQL type.
instance DBType Int64 where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.IntegerLit . fromIntegral
    , decode = fromField
    , typeName = "int8"
    }


-- | Corresponds to the @text@ PostgreSQL type.
instance DBType Text where
  typeInformation = dimap unpack fromString typeInformation


-- | Corresponds to the @text@ PostgreSQL type.
instance DBType String where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.StringLit
    , decode = fromField
    , typeName = "text"
    }


-- | Extends any @DBType@ with the value @null@. Note that you cannot "stack"
-- @Maybe@s, as SQL doesn't distinguish @Just Nothing@ from @Nothing@.
instance DBType a => DBType ( Maybe a ) where
  typeInformation = DatabaseType
    { encode = maybe (Opaleye.ConstExpr Opaleye.NullLit) encode
    , decode = optionalField decode
    , typeName
    }
    where
      DatabaseType{ encode, decode, typeName } = typeInformation


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


data ExprField a x where
  ExprField :: ExprField a a


-- | Any 'Expr' can be seen as a 'Table' with only one column.
instance Table ( Expr a ) where
  type Context ( Expr a ) =
    Expr

  type ConstrainTable ( Expr a ) c =
    c a

  type Field ( Expr a ) =
    ExprField a

  field expr ExprField =
    MkC expr

  tabulateMCP _ f =
    toColumn <$> f ExprField


instance Recontextualise ( Expr a ) Id where
  type MapTable Id ( Expr a ) = Expr a
  fieldMapping ExprField = ExprField
  reverseFieldMapping ExprField = ExprField


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


null_ :: Expr b -> ( Expr a -> Expr b ) -> Expr ( Maybe a ) -> Expr b
null_ whenNull f a =
 ifThenElse_ ( isNull a ) whenNull ( f ( retype a ) )


ifThenElse_ :: ExprTable a => Expr Bool -> a -> a -> a
ifThenElse_ bool whenTrue whenFalse =
  case_ [ ( bool, whenTrue ) ] whenFalse


case_ :: (Table a, Context a ~ Expr) => [ ( Expr Bool, a ) ] -> a -> a
case_ alts def =
  runIdentity $ tabulateMCP (Proxy @Unconstrained) \x ->
    pure $ MkC $ fromPrimExpr $
    Opaleye.CaseExpr
        [ ( toPrimExpr bool, toPrimExpr $ toColumn $ field alt x ) | ( bool, alt ) <- alts ]
        ( toPrimExpr $ toColumn $ field def x )


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


class (Table a, Context a ~ Expr) => ExprTable a
instance (Table a, Context a ~ Expr) => ExprTable a
