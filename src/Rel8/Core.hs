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

{-# options -fno-warn-deprecations #-}

module Rel8.Core where

import Control.Applicative ( liftA2 )
import Data.Aeson ( Value )
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import Data.CaseInsensitive ( CI )
import Data.Functor.Identity
import Data.Int
import Data.Kind
import Data.Kind ( Type )
import Data.Profunctor ( Profunctor(..), dimap )
import Data.Proxy ( Proxy( Proxy ) )
import Data.Scientific ( Scientific )
import Data.String
import Data.Text ( Text )
import qualified Data.Text.Lazy
import Data.Time ( Day, LocalTime, UTCTime, ZonedTime, TimeOfDay )
import Data.Typeable ( Typeable )
import Data.UUID ( UUID )
import Database.PostgreSQL.Simple.FromField ( FromField, FieldParser, fromField, optionalField, returnError, ResultError( Incompatible ) )
import Database.PostgreSQL.Simple.FromRow ( RowParser, fieldWith )
import GHC.Generics hiding ( C )
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Opaleye.PGTypes
import Rel8.Column
import Rel8.Unconstrained

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
class HigherKindedTable ( t :: ( Type -> Type ) -> Type ) where
  -- | Like 'Field', but for higher-kinded tables.
  type HField t = ( field :: Type -> Type ) | field -> t
  type HField t =
    GenericField t

  -- | Like 'Constraintable', but for higher-kinded tables.
  type HConstrainTable t ( f :: Type -> Type ) ( c :: Type -> Constraint ) :: Constraint
  type HConstrainTable t f c =
    GHConstrainTable ( Rep ( t f ) ) ( Rep ( t SPINE ) ) c

  -- | Like 'field', but for higher-kinded tables.
  hfield :: t f -> HField t x -> C f x
  default hfield
    :: forall f x
     . ( Generic ( t f )
       , HField t ~ GenericField t
       , GHigherKindedTable ( Rep ( t f ) ) t f ( Rep ( t SPINE ) )
       )
    => t f -> HField t x -> C f x
  hfield x ( GenericField i ) =
    ghfield @( Rep ( t f ) ) @t @f @( Rep ( t SPINE ) ) ( from x ) i

  -- | Like 'tabulateMCP', but for higher-kinded tables.
  htabulate
    :: ( Applicative m, HConstrainTable t f c )
    => proxy c -> ( forall x. c x => HField t x -> m ( C f x ) ) -> m ( t f )

  default htabulate
    :: forall f m c proxy
     . ( Applicative m, GHConstrainTable ( Rep ( t f ) ) ( Rep ( t SPINE ) ) c, Generic ( t f )
       , GHigherKindedTable ( Rep ( t f ) ) t f ( Rep ( t SPINE ) )
       , HField t ~ GenericField t
       )
    => proxy c -> ( forall x. c x => HField t x -> m ( C f x ) ) -> m ( t f )
  htabulate proxy f =
    fmap to ( ghtabulate @( Rep ( t f ) ) @t @f @( Rep ( t SPINE ) ) proxy ( f . GenericField ) )



data TableHField t ( f :: Type -> Type ) x where
  F :: HField t x -> TableHField t f x


-- | Any 'HigherKindedTable' is also a 'Table'.
instance HigherKindedTable t => Table (t f) where
  type Structure (t f) = t
  type Context (t f) = f
  toStructure = id
  fromStructure = id


data GenericField t a where
  GenericField :: GHField t ( Rep ( t SPINE ) ) a -> GenericField t a


class GHigherKindedTable ( rep :: Type -> Type ) ( t :: ( Type -> Type ) -> Type ) ( f :: Type -> Type ) ( repIdentity :: Type -> Type ) where
  data GHField t repIdentity :: Type -> Type

  type GHConstrainTable rep repIdentity ( c :: Type -> Constraint ) :: Constraint

  ghfield :: rep a -> GHField t repIdentity x -> C f x

  ghtabulate
    :: ( Applicative m, GHConstrainTable rep repIdentity c )
    => proxy c
    -> ( forall x. c x => GHField t repIdentity x -> m ( C f x ) )
    -> m ( rep a )


instance GHigherKindedTable x t f x' => GHigherKindedTable ( M1 i c x ) t f ( M1 i' c' x' ) where
  data GHField t ( M1 i' c' x' ) a where
    M1Field :: GHField t x' a -> GHField t ( M1 i' c' x' ) a

  type GHConstrainTable ( M1 i c x ) ( M1 i' c' x' ) constraint =
    GHConstrainTable x x' constraint

  ghfield ( M1 a ) ( M1Field i ) =
    ghfield a i

  ghtabulate proxy f =
    M1 <$> ghtabulate @x @t @f @x' proxy ( f . M1Field )


instance ( GHigherKindedTable x t f x', GHigherKindedTable y t f y' ) => GHigherKindedTable ( x :*: y ) t f ( x' :*: y' ) where
  data GHField t ( x' :*: y' ) a where
    FieldL :: GHField t x' a -> GHField t ( x' :*: y' ) a
    FieldR :: GHField t y' a -> GHField t ( x' :*: y' ) a

  type GHConstrainTable ( x :*: y ) ( x' :*: y' ) constraint =
    ( GHConstrainTable x x' constraint, GHConstrainTable y y' constraint )

  ghfield ( x :*: y ) = \case
    FieldL i -> ghfield x i
    FieldR i -> ghfield y i

  ghtabulate proxy f =
    (:*:) <$> ghtabulate @x @t @f @x' proxy ( f . FieldL )
          <*> ghtabulate @y @t @f @y' proxy ( f . FieldR )


type family IsColumnApplication ( a :: Type ) :: Bool where
  IsColumnApplication ( SPINE a ) = 'True
  IsColumnApplication _ = 'False



instance DispatchK1 ( IsColumnApplication c' ) f c c' => GHigherKindedTable ( K1 i c ) t f ( K1 i' c' ) where
  data GHField t ( K1 i' c' ) a where
    K1Field :: K1Field ( IsColumnApplication c' ) c' x -> GHField t ( K1 i' c' ) x

  type GHConstrainTable ( K1 i c ) ( K1 i' c' ) constraint =
    ConstrainK1 ( IsColumnApplication c' ) c c' constraint

  ghfield ( K1 a ) ( K1Field i ) =
    k1field @( IsColumnApplication c' ) @f @c @c' a i

  ghtabulate proxy f =
    K1 <$> k1tabulate @( IsColumnApplication c' ) @f @c @c' proxy ( f . K1Field )


class DispatchK1 ( isSPINE :: Bool ) f a a' where
  data K1Field isSPINE a' :: Type -> Type

  type ConstrainK1 isSPINE a a' ( c :: Type -> Constraint ) :: Constraint

  k1field :: a -> K1Field isSPINE a' x -> C f x

  k1tabulate
    :: ( ConstrainK1 isSPINE a a' c, Applicative m )
    => proxy c -> ( forall x. c x => K1Field isSPINE a' x -> m ( C f x ) ) -> m a


instance (a ~ Column f b) => DispatchK1 'True f a ( SPINE b ) where
  data K1Field 'True ( SPINE b ) x where
    K1True :: K1Field 'True ( SPINE b ) b

  type ConstrainK1 'True a ( SPINE b ) c =
    c b

  k1field a K1True =
    MkC a

  k1tabulate _ f =
    toColumn <$> f @b K1True


data SPINE a


{-| Types that represent SQL tables.

You generally should not need to derive instances of this class manually, as
writing higher-kinded data types is usually more convenient. See also:
'HigherKindedTable'.

-}
class HigherKindedTable (Structure t) => Table (t :: Type) where
  type Structure t :: (Type -> Type) -> Type
  type Context t :: Type -> Type

  toStructure :: t -> Structure t (Context t)
  fromStructure :: Structure t (Context t) -> t



-- | Effectfully map a table from one context to another.
traverseTableWithIndexC
  :: forall c t t' m
   . (Applicative m, Table t, Table t', HConstrainTable (Structure t') (Context t') c, Structure t ~ Structure t')
  => (forall x. c x => HField (Structure t) x -> C (Context t) x -> m (C (Context t') x))
  -> t
  -> m t'
traverseTableWithIndexC f t =
  fmap fromStructure $
  htabulate (Proxy @c) \index -> f index (hfield (toStructure t) index)


data HPair x y (f :: Type -> Type) = HPair { hfst :: x f, hsnd :: y f }
  deriving (Generic)


deriving instance (HigherKindedTable x, HigherKindedTable y) => HigherKindedTable (HPair x y)


instance (Table a, Table b, Context a ~ Context b) => Table (a, b) where
  type Structure (a, b) = HPair (Structure a) (Structure b)
  type Context (a, b) = Context a
  toStructure (a, b) = HPair (toStructure a) (toStructure b)
  fromStructure (HPair x y) = (fromStructure x, fromStructure y)


-- | Map a 'Table' from one type to another. The table types must be compatible,
-- see 'Compatible' for what that means.
mapTable
  :: forall t' t
   . (Table t, Table t', Structure t ~ Structure t', HConstrainTable (Structure t) (Context t') Unconstrained)
  => (forall x. C (Context t) x -> C (Context t') x) -> t -> t'
mapTable f = runIdentity . traverseTable (Identity . f)


-- | Map a 'Table' from one type to another, where all columns in the table are
-- subject to a constraint.
mapTableC
  :: forall c t' t
   . (Table t, Table t', Structure t ~ Structure t', HConstrainTable (Structure t) (Context t') c)
  => (forall x. c x => C (Context t) x -> C (Context t') x)
  -> t -> t'
mapTableC f =
  runIdentity . traverseTableC @c ( Identity . f )


-- | Effectfully traverse all fields in a 'Table', potentially producing another
-- @Table@.
traverseTable
  :: forall t' t m
   . (Applicative m, Table t, Table t', Structure t ~ Structure t', HConstrainTable (Structure t) (Context t') Unconstrained)
  => (forall x. C (Context t) x -> m (C (Context t') x))
  -> t
  -> m t'
traverseTable f =
  traverseTableWithIndexC @Unconstrained (const f)


-- | Effectfully traverse all fields in a 'Table', provided that all fields
-- satisfy a given constraint. For example, if all fields in a table have an
-- instance for 'Read', we can apply 'readMaybe' to all fields in the table,
-- failing if any read fails:
--
-- >>> traverseTableC @Read ( traverseC readMaybe ) MyTable{ fieldA = "True" }
-- Just MyTable{ fieldA = True }
traverseTableC
  :: forall c m t t'
   . (Table t, Table t', Applicative m, Structure t ~ Structure t', HConstrainTable (Structure t) (Context t') c)
  => (forall x. c x => C (Context t) x -> m (C (Context t') x))
  -> t
  -> m t'
traverseTableC f =
  traverseTableWithIndexC @c (const f)


zipTablesWithM
  :: forall t m
   . ( Applicative m
     , Table t
     , HConstrainTable (Structure t) (Context t) Unconstrained
     )
  => ( forall x. C ( Context t ) x -> C ( Context t ) x -> m ( C ( Context t ) x ) )
  -> t -> t -> m t
zipTablesWithM f t t' =
  fmap fromStructure $
  htabulate (Proxy @Unconstrained) \index ->
    f (hfield (toStructure t) index) (hfield (toStructure t') index)


instance (Context a ~ f, Table a, Structure a ~ Structure a') => DispatchK1 'False f a a' where
  data K1Field 'False a' x where
    K1False :: HField (Structure a') x -> K1Field 'False a' x

  type ConstrainK1 'False a a' c =
    HConstrainTable (Structure a) (Context a) c

  k1field a (K1False i) =
    hfield (toStructure a) i

  k1tabulate proxy f =
    fromStructure <$> htabulate proxy (f . K1False)


newtype HIdentity a f = HIdentity { unHIdentity :: Column f a }
  deriving ( Generic, HigherKindedTable )


-- | Any 'Identity' can be seen as a 'Table' with only one column.
instance Table (Identity a) where
  type Context (Identity a) = Identity
  type Structure (Identity a) = HIdentity a
  toStructure = HIdentity . runIdentity
  fromStructure = Identity . unHIdentity


-- | @Serializable@ witnesses the one-to-one correspondence between the type @sql@,
-- which contains SQL expressions, and the type @haskell@, which contains the
-- Haskell decoding of rows containing @sql@ SQL expressions.
class ExprTable sql => Serializable sql haskell | sql -> haskell, haskell -> sql where
  rowParser :: sql -> RowParser haskell
  lit :: haskell -> sql


type family Choose (a :: Type) (b :: Type) :: Choice where
  Choose (a, b) (x, y) = 'TABLE
  Choose (Expr a) a = 'EXPR
  Choose (t Expr) (t Identity) = 'TABLE
  Choose (MaybeTable a) (Maybe b) = 'TABLE


data Choice = EXPR | TABLE


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


instance (ExprTable a, SerializableChoice (Choose a b) a b, a ~ ExprType b, b ~ ResultType a) => Serializable a b where
  rowParser = rowParser_ (Proxy @(Choose a b))
  lit = lit_ (Proxy @(Choose a b))


class ExprTable a => SerializableChoice (choice :: Choice) (a :: Type) (b :: Type) where
  rowParser_ :: proxy choice -> a -> RowParser b
  lit_ :: proxy choice -> b -> a


-- | Any higher-kinded records can be @SELECT@ed, as long as we know how to
-- decode all of the records constituent part's.
instance (expr ~ Expr, identity ~ Identity, ExprTable (t expr), Table (t identity), HConstrainTable t Identity DBType) => SerializableChoice 'TABLE ( t expr ) ( t identity ) where
  rowParser_ _ =
    traverseTableC @DBType ( traverseCC @DBType \_ -> fieldWith ( decode typeInformation ) )

  lit_ _ =
    runIdentity . mapContext (Proxy @Lit) (Proxy @DBType) (Identity . mapCC @DBType lit)


instance DBType a => SerializableChoice 'EXPR (Expr a) a where
  rowParser_ _ _ = fieldWith (decode typeInformation)

  lit_ _ = Expr . Opaleye.CastExpr typeName . encode
    where
      DatabaseType{ encode, typeName } = typeInformation


instance (Serializable a1 b1, Serializable a2 b2) => SerializableChoice 'TABLE (a1, a2) (b1, b2) where
  rowParser_ _ (a, b) =
    liftA2 (,) (rowParser a) (rowParser b)

  lit_ _ (a, b) = (lit a, lit b)


instance
  ( Context a ~ Expr
  , Table a
  , HConstrainTable (Structure a) Expr Unconstrained
  , HConstrainTable (Structure a) Expr DBType
  , Serializable a b
  ) => SerializableChoice 'TABLE (MaybeTable a) (Maybe b) where

  rowParser_ _ ( MaybeTable _ t ) = do
    rowExists <- fieldWith ( decode typeInformation )

    case rowExists of
      Just True ->
        Just <$> rowParser t

      _ ->
        Nothing <$ traverseTableC @DBType @RowParser @_ @a nullField t

  lit_ _ = \case
    Nothing -> noTable
    Just x -> pure (lit x)


nullField :: forall x f. C f x -> RowParser ( C f x )
nullField x = x <$ fieldWith (\_ _ -> pure ())


-- | Typed SQL expressions
newtype Expr ( a :: Type ) =
  Expr { toPrimExpr :: Opaleye.PrimExpr }


type role Expr representational


instance ( IsString a, DBType a ) => IsString ( Expr a ) where
  fromString =
    lit . fromString


class (HConstrainTable (Structure a) Expr Unconstrained, Table a, Context a ~ Expr, HConstrainTable (Structure a) Expr DBType) => ExprTable a
instance (HConstrainTable (Structure a) Expr Unconstrained, Table a, Context a ~ Expr, HConstrainTable (Structure a) Expr DBType) => ExprTable a


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
        null_ (lit False) (\x' -> null_ (lit False) (\y' -> x' ||. y') y) x


instance Monad MaybeTable where
  MaybeTable t a >>= f = case f a of
    MaybeTable t' b -> MaybeTable (liftNull (or_ t t')) b
    where
      or_ x y =
        null_ (lit False) (\x' -> null_ (lit False) (\y' -> x' ||. y') y) x


data HMaybeTable g f =
  HMaybeTable
    { hnullTag :: Column f (Maybe Bool)
    , hcontents :: g f
    }
  deriving
    (Generic)


deriving instance (forall f. Table (g f)) => HigherKindedTable (HMaybeTable g)


instance (ExprTable a, HigherKindedTable (Structure a)) => Table (MaybeTable a) where
  type Structure (MaybeTable a) = HMaybeTable (Structure a)
  type Context (MaybeTable a) = Expr

  toStructure (MaybeTable x y) = HMaybeTable x (toStructure y)
  fromStructure (HMaybeTable x y) = MaybeTable x (fromStructure y)


maybeTable
  :: ExprTable b
  => b -> (a -> b) -> MaybeTable a -> b
maybeTable def f MaybeTable{ nullTag, table } =
  ifThenElse_ (null_ (lit False) id nullTag) (f table) def


noTable :: forall a. ExprTable a => MaybeTable a
noTable = MaybeTable tag t
  where
    tag :: Expr (Maybe Bool)
    tag = lit (Nothing :: Maybe Bool)

    t :: a
    t = fromStructure $ runIdentity $ htabulate (Proxy @DBType) f
      where
        f :: forall x i. DBType x => i x -> Identity (C Expr x)
        f _ = pure $ MkC $ unsafeCoerceExpr (lit (Nothing :: Maybe x) :: Expr (Maybe x))


instance Table (Expr a) where
  type Context (Expr a) = Expr
  type Structure (Expr a) = HIdentity a
  toStructure = HIdentity
  fromStructure = unHIdentity


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

class (ExprType a ~ Expr a, ResultType (Expr a) ~ a, ExprType (Maybe a) ~ Expr (Maybe a)) => DBType ( a :: Type ) where
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
    , typeName = showPGType (Proxy @b)
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
    , decode = \x y -> fmap g $ decode x y
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


unsafeCoerceExpr :: Expr a -> Expr b
unsafeCoerceExpr ( Expr x ) = Expr x


retype :: Expr a -> Expr b
retype =
  fromPrimExpr . toPrimExpr


isNull :: Expr ( Maybe a ) -> Expr Bool
isNull =
  fromPrimExpr . Opaleye.UnExpr Opaleye.OpIsNull . toPrimExpr


fromPrimExpr :: Opaleye.PrimExpr -> Expr a
fromPrimExpr =
  Expr


type family Apply (g :: (Type -> Type) -> Type -> Type) (f :: Type -> Type) :: Type -> Type where
  Apply Lit Identity = Expr
  Apply g f = g f


data Lit (f :: Type -> Type) a


class
  ( Table a
  , Table b
  , MapContext g a ~ b
  , Structure a ~ Structure b
  , Apply g (Context a) ~ Context b
  ) =>
    Recontextualise g a b
  where

  type MapContext (g :: (Type -> Type) -> Type -> Type) a :: Type

  mapContext :: (Applicative m, HConstrainTable (Structure a) (Context b) c)
    => proxy g
    -> proxy' c
    -> (forall x. c x => C (Context a) x -> m (C (Context b) x))
    -> a
    -> m b


instance (HigherKindedTable t, f' ~ Apply g f) => Recontextualise g (t f) (t f') where
  type MapContext g (t f) = t (Apply g f)

  mapContext _ c f as = htabulate c (\field -> f (hfield as field))


instance (Recontextualise g a a', Recontextualise g b b', Context a ~ Context b) => Recontextualise g (a, b) (a', b') where
  type MapContext g (a, b) = (MapContext g a, MapContext g b)

  mapContext g c f (a, b) = (,) <$> mapContext g c f a <*> mapContext g c f b


-- | FIXME: This isn't quite right
instance Recontextualise Lit (Identity a) (Expr a) where
  type MapContext Lit (Identity a) = Expr a

  mapContext _ _ f (Identity a) = unC <$> f (MkC a)
    where
      unC (MkC x) = x
