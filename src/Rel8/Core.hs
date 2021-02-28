{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
{-# language ViewPatterns #-}

{-# options -fno-warn-deprecations #-}

module Rel8.Core where

import Control.Applicative ( liftA2 )
import Data.Aeson ( ToJSON, FromJSON, parseJSON, toJSON, Value )
import Data.Aeson.Types ( parseEither )
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import Data.CaseInsensitive ( CI )
import Data.Functor.Identity ( Identity(runIdentity) )
import Data.Int ( Int32, Int64 )
import Data.Kind ( Type, Constraint )
import Data.Profunctor ( Profunctor(..), dimap )
import Data.Proxy ( Proxy( Proxy ) )
import Data.Scientific ( Scientific )
import Data.String ( IsString(..) )
import Data.Text ( Text )
import qualified Data.Text.Lazy
import Data.Time ( Day, LocalTime, UTCTime, ZonedTime, TimeOfDay )
import Data.Typeable ( Typeable )
import Data.UUID ( UUID )
import Database.PostgreSQL.Simple.FromField ( FromField, FieldParser, fromField, optionalField, returnError, ResultError( Incompatible ) )
import Database.PostgreSQL.Simple.FromRow ( RowParser, fieldWith )
import Data.Functor.Compose ( Compose(..) )
import GHC.Generics
    ( Generic(from, Rep), K1(K1), M1(M1), type (:*:)(..), unM1 )
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Opaleye.PGTypes
    ( pgBool,
      pgCiLazyText,
      pgCiStrictText,
      pgDay,
      pgDouble,
      pgInt4,
      pgInt8,
      pgLazyByteString,
      pgLazyText,
      pgLocalTime,
      pgNumeric,
      pgStrictByteString,
      pgStrictText,
      pgString,
      pgTimeOfDay,
      pgUTCTime,
      pgUUID,
      pgValueJSON,
      pgZonedTime,
      IsSqlType(..) )
import Rel8.Column ( sequenceC, C(..), Column )
import Text.Read ( readEither )


{-| Higher-kinded data types.

Higher-kinded data types are data types of the pattern:

@
data MyType f =
  MyType { field1 :: Column f T1 OR HK1 f
         , field2 :: Column f T2 OR HK2 f
         , ...
         , fieldN :: Column f Tn OR HKn f
         }
@

where @Tn@ is any Haskell type, and @HKn@ is any higher-kinded type.

That is, higher-kinded data are records where all fields in the record
are all either of the type @Column f T@ (for any @T@), or are themselves
higher-kinded data:

[Nested]

@
data Nested f =
  Nested { nested1 :: MyType f
         , nested2 :: MyType f
         }
@

The @HigherKindedTable@ type class is used to give us a special mapping
operation that lets us change the type parameter @f@.

[Supplying @HigherKindedTable@ instances]

This type class should be derived generically for all table types in your
project. To do this, enable the @DeriveAnyType@ and @DeriveGeneric@ language
extensions:

@
\{\-\# LANGUAGE DeriveAnyClass, DeriveGeneric #-\}
import qualified GHC.Generics

data MyType f = MyType { fieldA :: Column f T }
  deriving ( GHC.Generics.Generic, HigherKindedTable )
@

-}
class HigherKindedTable (t :: (Type -> Type) -> Type) where
  type HField t = (field :: Type -> Type) | field -> t
  type HField t = GenericHField t

  type HConstrainTable t (c :: Type -> Constraint) :: Constraint
  type HConstrainTable t c = HConstrainTable (Columns (Rep (t SPINE) ())) c

  hfield :: t f -> HField t x -> C f x
  htabulate :: forall f. (forall x. HField t x -> C f x) -> t f
  htraverse :: forall f g m. Applicative m => (forall x. C f x -> m (C g x)) -> t f -> m (t g)
  hdicts :: forall c. HConstrainTable t c => t (Dict c)

  default hfield :: forall f x. (Table f (Rep (t f) ()), Columns (Rep (t f) ()) ~ Columns (Rep (t SPINE) ()), Generic (t f), HField t ~ GenericHField t) => t f -> HField t x -> C f x
  hfield x (GenericHField i) =
    hfield (toColumns (from @_ @() x)) i
    -- ghfield @(Rep (t f)) @t @f @(Rep (t SPINE)) (from x) i

  default htabulate
    :: forall f. (forall x. HField t x -> C f x) -> t f
  htabulate _ = undefined
    -- ghtabulate @(Rep (t f)) @t @f @(Rep (t SPINE)) (from x) f

  default htraverse
    :: forall m f g. (forall x. C f x -> m (C g x)) -> t f -> m (t g)
  htraverse f = undefined -- ghtraverse @(Rep (t f)) @t @f @(Rep (t SPINE)) f

  default hdicts :: t (Dict c)
  hdicts = undefined


data Dict c a where
  Dict :: c a => Dict c a


data GenericHField t a where
  GenericHField :: HField (Columns (Rep (t SPINE) ())) a -> GenericHField t a


instance Table context (f a) => Table context (M1 i c f a) where
  type Columns (M1 i c f a) = Columns (f a)
  toColumns = toColumns . unM1
  fromColumns = M1 . fromColumns


instance (Table context (f a), Table context (g a)) => Table context ((:*:) f g a) where
  type Columns ((:*:) f g a) = HPair (Columns (f a)) (Columns (g a))
  toColumns (x :*: y) = HPair (toColumns x) (toColumns y)
  fromColumns (HPair x y) = fromColumns x :*: fromColumns y


-- class GHigherKindedTable (rep :: Type -> Type) (t :: (Type -> Type) -> Type) (f :: Type -> Type) (repIdentity :: Type -> Type) where
--   data GHField t repIdentity :: Type -> Type

--   type GHConstrainTable repIdentity (c :: Type -> Constraint) :: Constraint

--   ghfield :: rep a -> GHField t repIdentity x -> C f x
--   ghtraverse :: Applicative m => (forall x. C f x -> m (C g x)) -> rep a -> m (rep b)


-- instance GHigherKindedTable x t f x' => GHigherKindedTable (M1 i c x) t f (M1 i' c' x') where
--   data GHField t (M1 i' c' x') a where
--     M1Field :: GHField t x' a -> GHField t (M1 i' c' x') a

--   type GHConstrainTable (M1 i' c' x') constraint =
--     GHConstrainTable x' constraint

--   ghfield (M1 a) (M1Field i) = ghfield a i

--   ghtabulate f = M1 $ ghtabulate @x @t @f @x' (f . M1Field)


-- instance (GHigherKindedTable x t f x', GHigherKindedTable y t f y') => GHigherKindedTable (x :*: y) t f (x' :*: y') where
--   data GHField t (x' :*: y') a where
--     FieldL :: GHField t x' a -> GHField t (x' :*: y') a
--     FieldR :: GHField t y' a -> GHField t (x' :*: y') a

--   type GHConstrainTable (x' :*: y') constraint =
--     (GHConstrainTable x' constraint, GHConstrainTable y' constraint)

--   ghfield (x :*: y) = \case
--     FieldL i -> ghfield x i
--     FieldR i -> ghfield y i

--   ghtabulate f =
--     ghtabulate @x @t @f @x' (f . FieldL) :*: ghtabulate @y @t @f @y' (f . FieldR)


-- type family IsColumnApplication (a :: Type) :: Bool where
--   IsColumnApplication (SPINE a) = 'True
--   IsColumnApplication _         = 'False


-- instance DispatchK1 (IsColumnApplication c') f c c' => GHigherKindedTable (K1 i c) t f (K1 i' c') where
--   data GHField t (K1 i' c') a where
--     K1Field :: K1Field (IsColumnApplication c') c' x -> GHField t (K1 i' c') x

--   type GHConstrainTable (K1 i' c') constraint =
--     ConstrainK1 (IsColumnApplication c') c' constraint

--   ghfield (K1 a) (K1Field i) =
--     k1field @(IsColumnApplication c') @f @c @c' a i

--   ghtabulate f =
--     K1 $ k1tabulate @(IsColumnApplication c') @f @c @c' (f . K1Field)


-- class DispatchK1 (isSPINE :: Bool) f a a' where
--   data K1Field isSPINE a' :: Type -> Type

--   type ConstrainK1 isSPINE a' (c :: Type -> Constraint) :: Constraint

--   k1field :: a -> K1Field isSPINE a' x -> C f x

--   k1tabulate :: (forall x. K1Field isSPINE a' x -> C f x) -> a


-- instance (a ~ Column f b) => DispatchK1 'True f a (SPINE b) where
--   data K1Field 'True (SPINE b) x where
--     K1True :: K1Field 'True (SPINE b) b

--   type ConstrainK1 'True (SPINE b) c = c b

--   k1field a K1True = MkC a

--   k1tabulate f = toColumn $ f @b K1True


data SPINE a


{-| Types that represent SQL tables.

You generally should not need to derive instances of this class manually, as
writing higher-kinded data types is usually more convenient. See also:
'HigherKindedTable'.

-}
class HigherKindedTable (Columns t) => Table (context :: Type -> Type) (t :: Type) | t -> context where
  type Columns t :: (Type -> Type) -> Type

  toColumns :: t -> Columns t context
  fromColumns :: Columns t context -> t


-- | Any 'HigherKindedTable' is also a 'Table'.
instance (HigherKindedTable t, f ~ g) => Table f (t g) where
  type Columns (t g) = t
  toColumns = id
  fromColumns = id


data HPair x y (f :: Type -> Type) = HPair { hfst :: x f, hsnd :: y f }
  deriving (Generic)


deriving instance (HigherKindedTable x, HigherKindedTable y) => HigherKindedTable (HPair x y)


instance (Table f a, Table f b) => Table f (a, b) where
  type Columns (a, b) = HPair (Columns a) (Columns b)
  toColumns (a, b) = HPair (toColumns a) (toColumns b)
  fromColumns (HPair x y) = (fromColumns x, fromColumns y)


instance context ~ Expr => Table context (K1 i a x) where


-- instance (Table f a, Columns a ~ Columns a') => DispatchK1 'False f a a' where
--   data K1Field 'False a' x where
--     K1False :: HField (Columns a') x -> K1Field 'False a' x

--   type ConstrainK1 'False a' c =
--     HConstrainTable (Columns a') c

--   k1field a (K1False i) =
--     hfield (toColumns a) i

--   k1tabulate f =
--     fromColumns $ htabulate (f . K1False)


newtype HIdentity a f = HIdentity { unHIdentity :: Column f a }
  deriving ( Generic, HigherKindedTable )


-- | @Serializable@ witnesses the one-to-one correspondence between the type @sql@,
-- which contains SQL expressions, and the type @haskell@, which contains the
-- Haskell decoding of rows containing @sql@ SQL expressions.
class SerializationMethod sql haskell => Serializable sql haskell | sql -> haskell, haskell -> sql where
  lit :: haskell -> sql

instance SerializationMethod a b => Serializable a b where
  lit = litImpl


type family ExprType (a :: Type) :: Type where
  ExprType (a, b) = (ExprType a, ExprType b)
  ExprType (t Identity) = t Expr
  ExprType (Maybe (t Identity)) = MaybeTable (t Expr)
  ExprType (Maybe a) = Expr (Maybe a)
  ExprType a = Expr a


type family ResultType (a :: Type) :: Type where
  ResultType (a, b) = (ResultType a, ResultType b)
  ResultType (t Expr) = t Identity
  ResultType (Expr a) = a
  ResultType (MaybeTable a) = Maybe (ResultType a)


class (Table Expr expr, expr ~ ExprType haskell, haskell ~ ResultType expr) => SerializationMethod (expr :: Type) (haskell :: Type) where
  rowParser :: RowParser haskell
  litImpl :: haskell -> expr


-- | Any higher-kinded records can be @SELECT@ed, as long as we know how to
-- decode all of the records constituent part's.
instance (s ~ t, expr ~ Expr, identity ~ Identity, HigherKindedTable t, HConstrainTable t DBType) => SerializationMethod (s expr) (t identity) where
  rowParser = htraverse sequenceC $ htabulate @t (f . hfield (hdicts @t)) 
    where
      f :: forall a. C (Dict DBType) a -> C (Compose RowParser Identity) a
      f (MkC Dict) = MkC $ fieldWith $ decode $ typeInformation @a

  litImpl t =
    fromColumns $ htabulate \i ->
      case (hfield (hdicts @t @DBType) i, hfield t i) of
        (MkC Dict, MkC x) -> MkC $ lit x 


instance (DBType a, a ~ b) => SerializationMethod (Expr a) b where
  rowParser = fieldWith (decode typeInformation)

  litImpl = Expr . Opaleye.CastExpr typeName . encode
    where
      DatabaseType{ encode, typeName } = typeInformation


instance (Serializable a1 b1, Serializable a2 b2) => SerializationMethod (a1, a2) (b1, b2) where
  rowParser = liftA2 (,) rowParser rowParser

  litImpl (a, b) = (lit a, lit b)


instance (Table Expr a, ExprType (Maybe b) ~ MaybeTable a, ResultType a ~ b, SerializationMethod a b, HConstrainTable (Columns a) DBType) => SerializationMethod (MaybeTable a) (Maybe b) where
  rowParser = do
    rowExists <- fieldWith ( decode typeInformation )

    case rowExists of
      Just True -> Just <$> rowParser
      _         -> Nothing <$ htraverse nullField (hdicts @(Columns a) @DBType)

  litImpl = \case
    Nothing -> noTable
    Just x  -> pure $ lit x


nullField :: forall x f. C f x -> RowParser (C f x)
nullField x = x <$ fieldWith (\_ _ -> pure ())


-- | Typed SQL expressions
newtype Expr (a :: Type) = Expr { toPrimExpr :: Opaleye.PrimExpr }


type role Expr representational


instance ( IsString a, DBType a ) => IsString ( Expr a ) where
  fromString =
    lit . fromString


{-| @MaybeTable t@ is the table @t@, but as the result of an outer join. If the
outer join fails to match any rows, this is essentialy @Nothing@, and if the
outer join does match rows, this is like @Just@. Unfortunately, SQL makes it
impossible to distinguish whether or not an outer join matched any rows based
generally on the row contents - if you were to join a row entirely of nulls,
you can't distinguish if you matched an all null row, or if the match failed.
For this reason @MaybeTable@ contains an extra field - 'nullTag' - to
track whether or not the outer join produced any rows.

-}
data MaybeTable t where
  MaybeTable
    :: { -- | Check if this @MaybeTable@ is null. In other words, check if an outer
         -- join matched any rows.
         nullTag :: Expr ( Maybe Bool )
       , table :: t
       }
    -> MaybeTable t
  deriving
    ( Functor )


instance Applicative MaybeTable where
  pure = MaybeTable (lit (Just True))
  MaybeTable t f <*> MaybeTable t' a = MaybeTable (liftNull (or_ t t')) (f a)
    where
      or_ x y =
        null_ (lit False) (\x' -> null_ (lit False) (x' ||.) y) x


instance Monad MaybeTable where
  MaybeTable t a >>= f = case f a of
    MaybeTable t' b -> MaybeTable (liftNull (or_ t t')) b
    where
      or_ x y =
        null_ (lit False) (\x' -> null_ (lit False) (x' ||.) y) x


data HMaybeTable g f =
  HMaybeTable
    { hnullTag :: Column f (Maybe Bool)
    , hcontents :: g f
    }
  deriving
    (Generic)


deriving instance (forall f. Table f (g f)) => HigherKindedTable (HMaybeTable g)


instance Table Expr a => Table Expr (MaybeTable a) where
  type Columns (MaybeTable a) = HMaybeTable (Columns a)

  toColumns (MaybeTable x y) = HMaybeTable x (toColumns y)
  fromColumns (HMaybeTable x y) = MaybeTable x (fromColumns y)


maybeTable
  :: Table Expr b
  => b -> (a -> b) -> MaybeTable a -> b
maybeTable def f MaybeTable{ nullTag, table } =
  ifThenElse_ (null_ (lit False) id nullTag) (f table) def


noTable :: forall a. (Table Expr a, HConstrainTable (Columns a) DBType) => MaybeTable a
noTable = MaybeTable (lit Nothing) $ fromColumns $ htabulate f
  where
    f :: forall x. HField (Columns a) x -> C Expr x
    f i = 
      case hfield (hdicts @(Columns a) @DBType) i of 
        MkC Dict -> MkC $ unsafeCoerceExpr (lit (Nothing :: Maybe x))


instance expr ~ Expr => Table expr (Expr a) where
  type Columns (Expr a) = HIdentity a
  toColumns = HIdentity
  fromColumns = unHIdentity


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

-}

class (ExprType a ~ Expr a, ResultType (Expr a) ~ a, ExprType (Maybe a) ~ Expr (Maybe a), Serializable (Expr a) a) => DBType (a :: Type) where
  typeInformation :: DatabaseType a a


data DatabaseType a b =
  DatabaseType
    { encode :: a -> Opaleye.PrimExpr
    , decode :: FieldParser b
    , typeName :: String
    }


fromOpaleye :: forall a b. (FromField a, IsSqlType b) => (a -> Opaleye.Column b) -> DatabaseType a a
fromOpaleye f =
  DatabaseType
    { encode = \x -> case f x of Opaleye.Column e -> e
    , decode = fromField
    , typeName = showSqlType (Proxy @b)
    }


parseDatabaseType :: Typeable b => (a -> Either String b) -> DatabaseType i a -> DatabaseType i b
parseDatabaseType f DatabaseType{ encode, decode, typeName } =
  DatabaseType
    { encode = encode
    , decode = \x y -> decode x y >>= either (returnError Incompatible x) return . f
    , typeName
    }


instance Profunctor DatabaseType where
  dimap f g DatabaseType{ encode, decode, typeName } = DatabaseType
    { encode = encode . f
    , decode = \x y -> g <$> decode x y
    , typeName
    }


-- | Corresponds to the @bool@ PostgreSQL type.
instance DBType Bool where
  typeInformation = fromOpaleye pgBool


-- | Corresponds to the @int4@ PostgreSQL type.
instance DBType Int32 where
  typeInformation = dimap fromIntegral fromIntegral $ fromOpaleye pgInt4


-- | Corresponds to the @int8@ PostgreSQL type.
instance DBType Int64 where
  typeInformation = fromOpaleye pgInt8


instance DBType Float where
  typeInformation = DatabaseType
    { encode = Opaleye.ConstExpr . Opaleye.NumericLit . realToFrac
    , decode = \x y -> fromRational <$> fromField x y
    , typeName = "float4"
    }


instance DBType UTCTime where
  typeInformation = fromOpaleye pgUTCTime


-- | Corresponds to the @text@ PostgreSQL type.
instance DBType Text where
  typeInformation = fromOpaleye pgStrictText


-- | Corresponds to the @text@ PostgreSQL type.
instance DBType Data.Text.Lazy.Text where
  typeInformation = fromOpaleye pgLazyText


-- | Corresponds to the @text@ PostgreSQL type.
instance DBType String where
  typeInformation = fromOpaleye pgString


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


-- | Corresponds to the @json@ PostgreSQL type.
instance DBType Value where
  typeInformation = fromOpaleye pgValueJSON


instance DBType Data.ByteString.Lazy.ByteString where
  typeInformation = fromOpaleye pgLazyByteString


instance DBType Data.ByteString.ByteString where
  typeInformation = fromOpaleye pgStrictByteString


instance DBType Scientific where
  typeInformation = fromOpaleye pgNumeric


instance DBType Double where
  typeInformation = fromOpaleye pgDouble


instance DBType UUID where
  typeInformation = fromOpaleye pgUUID


instance DBType Day where
  typeInformation = fromOpaleye pgDay


instance DBType LocalTime where
  typeInformation = fromOpaleye pgLocalTime


instance DBType ZonedTime where
  typeInformation = fromOpaleye pgZonedTime


instance DBType TimeOfDay where
  typeInformation = fromOpaleye pgTimeOfDay


instance DBType (CI Text) where
  typeInformation = fromOpaleye pgCiStrictText


instance DBType (CI Data.Text.Lazy.Text) where
  typeInformation = fromOpaleye pgCiLazyText


liftNull :: Expr a -> Expr ( Maybe a )
liftNull =
  retype


null_ :: DBType b => Expr b -> ( Expr a -> Expr b ) -> Expr ( Maybe a ) -> Expr b
null_ whenNull f a =
 ifThenElse_ ( isNull a ) whenNull ( f ( retype a ) )


-- | The SQL @OR@ operator.
infixr 2 ||.
(||.) :: Expr Bool -> Expr Bool -> Expr Bool
(||.) ( Expr a ) ( Expr b ) =
    Expr ( Opaleye.BinExpr Opaleye.OpOr a b )


ifThenElse_ :: Table Expr a => Expr Bool -> a -> a -> a
ifThenElse_ bool whenTrue = case_ [ ( bool, whenTrue ) ]


case_ :: forall a. Table Expr a => [ ( Expr Bool, a ) ] -> a -> a
case_ alts def =
  fromColumns $ htabulate @(Columns a) \x -> MkC $ fromPrimExpr $
    Opaleye.CaseExpr
        [ ( toPrimExpr bool, toPrimExpr $ toColumn $ hfield (toColumns alt) x ) | ( bool, alt ) <- alts ]
        ( toPrimExpr $ toColumn $ hfield (toColumns def) x )


unsafeCoerceExpr :: Expr a -> Expr b
unsafeCoerceExpr (Expr x) = Expr x


retype :: Expr a -> Expr b
retype = fromPrimExpr . toPrimExpr


isNull :: Expr (Maybe a) -> Expr Bool
isNull = fromPrimExpr . Opaleye.UnExpr Opaleye.OpIsNull . toPrimExpr


fromPrimExpr :: Opaleye.PrimExpr -> Expr a
fromPrimExpr = Expr


-- | A deriving-via helper type for column types that store a Haskell value
-- using a JSON encoding described by @aeson@'s 'ToJSON' and 'FromJSON' type
-- classes.
newtype JSONEncoded a = JSONEncoded { fromJSONEncoded :: a }


instance (FromJSON a, ToJSON a, Typeable a) => DBType (JSONEncoded a) where
  typeInformation =
    parseDatabaseType (fmap JSONEncoded . parseEither parseJSON) $
      lmap (toJSON . fromJSONEncoded) typeInformation


-- | A deriving-via helper type for column types that store a Haskell value
-- using a Haskell's 'Read' and 'Show' type classes.
newtype ReadShow a = ReadShow { fromReadShow :: a }


instance (Read a, Show a, Typeable a) => DBType (ReadShow a) where
  typeInformation =
    parseDatabaseType (fmap ReadShow . readEither) $
      lmap (show . fromReadShow) typeInformation


pureTable :: Table f t => (forall x. C f x) -> t
pureTable f = fromColumns $ htabulate (const f)


mapTable
  :: (Columns s ~ Columns t, Table f s, Table g t)
  => (forall x. C f x -> C g x) -> s -> t
mapTable f = fromColumns . runIdentity . htraverse (pure . f) . toColumns


zipTablesWith
  :: (Table f x, Table g y, Table h z, Columns x ~ Columns y, Columns y ~ Columns z)
  => (forall a. C f a -> C g a -> C h a) -> x -> y -> z
zipTablesWith f (toColumns -> x) (toColumns -> y) =
  fromColumns $ htabulate $ liftA2 f (hfield x) (hfield y)


zipTablesWithM
  :: forall x y z f g h m
   . (Columns x ~ Columns y, Columns y ~ Columns z, Table f x, Table g y, Table h z, Applicative m)
  => (forall a. C f a -> C g a -> m (C h a)) -> x -> y -> m z
zipTablesWithM f (toColumns -> x) (toColumns -> y) =
  fmap fromColumns $
    htraverse sequenceC $
      htabulate @_ @(Compose m h) $
        MkC . fmap toColumn . liftA2 f (hfield x) (hfield y)


traverseTable
  :: (Columns x ~ Columns y, Table f x, Table g y, Applicative m)
  => (forall a. C f a -> m (C g a)) -> x -> m y
traverseTable f = fmap fromColumns . htraverse f . toColumns
