{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language InstanceSigs #-}
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

module Rel8
  ( -- * Database types
    DBType(..)
  , DBEq(..)
  , DatabaseType(..)
  , parseDatabaseType
    
    -- ** Deriving-via helpers
  , JSONEncoded(..)
  , ReadShow(..)

    -- * Expressions
  , Expr
  , null_
  , isNull
  , binaryOperator
  , unsafeCoerceExpr
  , liftNull
  , (&&.)
  , and_
  , (||.)
  , or_
  , catMaybe
  , not_
  , ifThenElse_
  , (==.)

    -- ** Functions
  , Function
  , function
  , nullaryFunction
   
    -- * Tables and higher-kinded tables
  , Table(..)
  , HigherKindedTable(..)

    -- * Table schemas
  , Column
  , TableSchema(..)
  , ColumnSchema

    -- * Queries
  , Query
  , each
  , values
  , filter
  , where_
  , limit
  , offset
  , union
  , distinct
  , exists
  , optional
  , MaybeTable
  , maybeTable
  , noTable
  , catMaybeTable

    -- * IO
  , Serializable
  , lit

    -- * Running statements
    -- ** @SELECT@
  , select
   
    -- ** @INSERT@
  , Insert(..)
  , OnConflict(..)
  , insert

    -- ** @DELETE@
  , Delete(..)
  , delete

    -- * @UPDATE@
  , update
  , Update(..)

    -- * @.. RETURNING@
  , Returning(..)
  )
  where

-- aeson
import Data.Aeson ( FromJSON, ToJSON, Value, parseJSON, toJSON )
import Data.Aeson.Types ( parseEither )

-- base
import Prelude hiding ( filter )
import Control.Applicative ( liftA2 )

import Data.Proxy ( Proxy( Proxy ) )
import Data.String ( IsString(..) )
import Data.Typeable ( Typeable )
import GHC.Generics ( (:*:)(..), Generic, K1(..), M1(..), Rep, from, to )
import Text.Read ( readEither )

-- bytestring
import qualified Data.ByteString
import qualified Data.ByteString.Lazy

-- case-insensitive
import Data.CaseInsensitive ( CI )

-- opaleye
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Opaleye.PGTypes
  ( IsSqlType(..)
  , pgBool
  , pgCiLazyText
  , pgCiStrictText
  , pgDay
  , pgDouble
  , pgInt4
  , pgInt8
  , pgLazyByteString
  , pgLazyText
  , pgLocalTime
  , pgNumeric
  , pgStrictByteString
  , pgStrictText
  , pgString
  , pgTimeOfDay
  , pgUTCTime
  , pgUUID
  , pgValueJSON
  , pgZonedTime
  )

-- postgresql-simple
import Database.PostgreSQL.Simple.FromField
  ( FieldParser
  , FromField
  , ResultError( Incompatible )
  , fromField
  , optionalField
  , returnError
  )
import Database.PostgreSQL.Simple.FromRow ( RowParser, fieldWith )

-- profunctors
import Data.Profunctor ( Profunctor(..), dimap )

-- scientific
import Data.Scientific ( Scientific )

-- text
import Data.Text ( Text )
import qualified Data.Text.Lazy

-- time
import Data.Time ( Day, LocalTime, TimeOfDay, UTCTime, ZonedTime )

-- uuid
import Data.UUID ( UUID )

import Data.Functor.Compose ( Compose )
import Data.Functor.Identity ( Identity(runIdentity) )
import Data.Kind ( Type, Constraint )


import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Foldable ( Foldable(toList, foldl') )
import Data.Int ( Int32, Int64 )

import qualified Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple ( Connection )
import qualified Database.PostgreSQL.Simple.FromRow as Database.PostgreSQL.Simple
import Numeric.Natural ( Natural )
import qualified Opaleye ( runInsert_, Insert(..), OnConflict(..), runDelete_, Delete(..), runUpdate_, Update(..), valuesExplicit )
import qualified Opaleye.Binary as Opaleye
import qualified Opaleye.Distinct as Opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.Binary as Opaleye

import qualified Opaleye.Internal.Distinct as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye ()
import qualified Opaleye.Internal.Manipulation as Opaleye
import qualified Opaleye.Internal.Optimize as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye hiding ( BinOp, limit )
import qualified Opaleye.Internal.Print as Opaleye ( formatAndShowSQL )
import qualified Opaleye.Internal.QueryArr as Opaleye
import qualified Opaleye.Internal.RunQuery as Opaleye
import qualified Opaleye.Internal.Table as Opaleye
import qualified Opaleye.Internal.Tag as Opaleye
import qualified Opaleye.Internal.Unpackspec as Opaleye
import qualified Opaleye.Internal.Values as Opaleye
import qualified Opaleye.Lateral as Opaleye
import qualified Opaleye.Operators as Opaleye hiding ( restrict )
import qualified Opaleye.Order as Opaleye
import qualified Opaleye.Table as Opaleye

import Rel8.ColumnSchema ( ColumnSchema(..) )

import qualified Rel8.Optimize
import Rel8.TableSchema
    ( toOpaleyeTable, TableSchema(..) )



{-| The @Column@ type family should be used to indicate which fields of your
data types are single columns in queries. This type family has special support
when a query is executed, allowing you to use a single data type for both query
data and rows decoded to Haskell.

To understand why this type family is special, let's consider a simple
higher-kinded data type of Haskell packages:

@
data HaskellPackage f = HaskellPackage
  { packageName   :: Column f String
  , packageAuthor :: Column f String
  }
@

In queries, @f@ will be some type of 'Rel8.Expr', and @Column Expr a@
reduces to just @Expr a@:

>>> :t packageName (package :: Package Expr)
Expr String

When we 'Rel8.Query.select' queries of this type, @f@ will be instantiated as
@Identity@, at which point all wrapping entire disappears:

>>> :t packageName (package :: Package Identity)
String

In @rel8@ we try hard to always know what @f@ is, which means holes should
mention precise types, rather than the @Column@ type family. You should only
need to be aware of the type family when defining your table types.
-}
type family Column (context :: Type -> Type) (a :: Type) :: Type where
  Column Identity a      = a
  Column (Compose f g) a = f (Column g a) -- TODO: Can we drop this and just use a Column (Compose f g) a?
  Column f a             = f a


-- | The @C@ newtype simply wraps 'Column', but this allows us to work around
-- injectivity problems of functions that return type family applications.
newtype C f x = MkC { toColumn :: Column f x }


-- | Lift functions that map between 'Column's to functions that map between
-- 'C's.
mapC :: (Column f x -> Column g y) -> C f x -> C g y
mapC f (MkC x) = MkC $ f x 


-- | Effectfully map from one column to another.
traverseC
  :: Applicative m
  => ( Column f x -> m ( Column g y ) ) -> C f x -> m ( C g y )
traverseC f ( MkC x ) =
  MkC <$> f x


-- | If a column contains an effectful operation, sequence that operation into a
--  new column.
sequenceC
  :: ( Column f a ~ m ( Column g y ), Functor m )
  => C f a -> m ( C g y )
sequenceC ( MkC x ) =
  MkC <$> x


-- | Zip two columns together under an effectful context.
zipCWithM
  :: Applicative m
  => ( Column f x -> Column g y -> m ( Column h z ) )
  -> C f x -> C g y -> m ( C h z )
zipCWithM f ( MkC x ) ( MkC y ) =
  MkC <$> f x y


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

data MyType f = MyType { fieldA :: Column f T }
  deriving ( GHC.Generics.Generic, HigherKindedTable )
@

-}
class HigherKindedTable (t :: (Type -> Type) -> Type) where
  type HField t = (field :: Type -> Type) | field -> t
  type HConstrainTable t (c :: Type -> Constraint) :: Constraint

  hfield :: t f -> HField t x -> C f x
  htabulate :: forall f. (forall x. HField t x -> C f x) -> t f
  htraverse :: forall f g m. Applicative m => (forall x. C f x -> m (C g x)) -> t f -> m (t g)
  hdicts :: forall c. HConstrainTable t c => t (Dict c)

  type HField t = GenericHField t
  type HConstrainTable t c = HConstrainTable (Columns (WithShape SPINE (Rep (t SPINE)) (Rep (t SPINE) ()))) c

  default hfield
    :: forall f x
     . ( Generic (t f)
       , HField t ~ GenericHField t
       , Columns (WithShape f (Rep (t SPINE)) (Rep (t f) ())) ~ Columns (WithShape SPINE (Rep (t SPINE)) (Rep (t f) ()))
       , HField (Columns (WithShape SPINE (Rep (t SPINE)) (Rep (t f) ()))) ~ HField (Columns (WithShape SPINE (Rep (t SPINE)) (Rep (t SPINE) ())))
       , HigherKindedTable (Columns (WithShape SPINE (Rep (t SPINE)) (Rep (t f) ())))
       , Table f (WithShape f (Rep (t SPINE)) (Rep (t f) ()))
       )
    => t f -> HField t x -> C f x
  hfield x (GenericHField i) =
    hfield (toColumns (WithShape @f @(Rep (t SPINE)) (GHC.Generics.from @_ @() x))) i

  default htabulate
    :: forall f
     . ( Generic (t f)
       , HField t ~ GenericHField t
       , Columns (WithShape f (Rep (t SPINE)) (Rep (t f) ())) ~ Columns (WithShape SPINE (Rep (t SPINE)) (Rep (t f) ()))
       , HField (Columns (WithShape SPINE (Rep (t SPINE)) (Rep (t f) ()))) ~ HField (Columns (WithShape SPINE (Rep (t SPINE)) (Rep (t SPINE) ())))
       , HigherKindedTable (Columns (WithShape SPINE (Rep (t SPINE)) (Rep (t f) ())))
       , Table f (WithShape f (Rep (t SPINE)) (Rep (t f) ()))
       )
    => (forall a. HField t a -> C f a) -> t f
  htabulate f =
    to @_ @() $ forgetShape @f @(Rep (t SPINE)) $ fromColumns $ htabulate (f . GenericHField)

  default htraverse
    :: forall f g m
     . ( Applicative m
       , Generic (t f)
       , Generic (t g)
       , Columns (WithShape f (Rep (t SPINE)) (Rep (t f) ())) ~ Columns (WithShape SPINE (Rep (t SPINE)) (Rep (t f) ()))
       , HigherKindedTable (Columns (WithShape SPINE (Rep (t SPINE)) (Rep (t f) ())))
       , Table f (WithShape f (Rep (t SPINE)) (Rep (t f) ()))
       , Table g (WithShape g (Rep (t SPINE)) (Rep (t g) ()))
       , Columns (WithShape g (Rep (t SPINE)) (Rep (t g) ())) ~ Columns (WithShape SPINE (Rep (t SPINE)) (Rep (t f) ()))
       )
    => (forall a. C f a -> m (C g a)) -> t f -> m (t g)
  htraverse f x =
    fmap (to @_ @() . forgetShape @g @(Rep (t SPINE)) . fromColumns)
      $ htraverse f
      $ toColumns
      $ WithShape @f @(Rep (t SPINE))
      $ GHC.Generics.from @_ @() x

  default hdicts
    :: forall c
     . ( Generic (t (Dict c))
       , Table (Dict c) (WithShape (Dict c) (Rep (t SPINE)) (Rep (t (Dict c)) ()))
       , HConstrainTable (Columns (WithShape (Dict c) (Rep (t SPINE)) (Rep (t (Dict c)) ()))) c
       )
    => t (Dict c)
  hdicts =
    to @_ @() $
      forgetShape @(Dict c) @(Rep (t SPINE)) $
        fromColumns $
          hdicts @(Columns (WithShape (Dict c) (Rep (t SPINE)) (Rep (t (Dict c)) ()))) @c


data Dict c a where
  Dict :: c a => Dict c a


data SPINE a


data GenericHField t a where
  GenericHField :: HField (Columns (WithShape SPINE (Rep (t SPINE)) (Rep (t SPINE) ()))) a -> GenericHField t a


newtype WithShape (context :: Type -> Type) (shape :: Type -> Type) a = WithShape { forgetShape :: a }


instance (context ~ context', Table context (WithShape context f (g a))) => Table context (WithShape context' (M1 i c f) (M1 i c g a)) where
  type Columns (WithShape context' (M1 i c f) (M1 i c g a)) = Columns (WithShape context' f (g a))
  toColumns = toColumns . WithShape @context @f . unM1 . forgetShape
  fromColumns = WithShape . M1 . forgetShape @context @f . fromColumns


instance (context ~ context', Table context (WithShape context shapeL (l a)), Table context (WithShape context shapeR (r a))) => Table context (WithShape context' (shapeL :*: shapeR) ((:*:) l r a)) where
  type Columns (WithShape context' (shapeL :*: shapeR) ((:*:) l r a)) =
    HPair
      (Columns (WithShape context' shapeL (l a)))
      (Columns (WithShape context' shapeR (r a)))

  toColumns (WithShape (x :*: y)) = HPair (toColumns (WithShape @context @shapeL x)) (toColumns (WithShape @context @shapeR y))
  fromColumns (HPair x y) = WithShape $ forgetShape @context @shapeL (fromColumns x) :*: forgetShape @context @shapeR (fromColumns y)


instance (context ~ context', K1Helper (IsColumnApplication shape) context shape b) => Table context (WithShape context' (K1 i shape) (K1 i b x)) where
  type Columns (WithShape context' (K1 i shape) (K1 i b x)) = K1Columns (IsColumnApplication shape) shape b
  toColumns = toColumnsHelper @(IsColumnApplication shape) @context @shape @b . unK1 . forgetShape
  fromColumns = WithShape . K1 . fromColumnsHelper @(IsColumnApplication shape) @context @shape @b


type family IsColumnApplication (a :: Type) :: Bool where
  IsColumnApplication (SPINE a) = 'True
  IsColumnApplication _         = 'False


class (isColumnApplication ~ IsColumnApplication shape, HigherKindedTable (K1Columns isColumnApplication shape a)) => K1Helper (isColumnApplication :: Bool) (context :: Type -> Type) (shape :: Type) (a :: Type) where
  type K1Columns isColumnApplication shape a :: (Type -> Type) -> Type
  toColumnsHelper :: a -> K1Columns isColumnApplication shape a context
  fromColumnsHelper :: K1Columns isColumnApplication shape a context -> a


instance (Table context a, IsColumnApplication shape ~ 'False) => K1Helper 'False context shape a where
  type K1Columns 'False shape a = Columns a
  toColumnsHelper = toColumns
  fromColumnsHelper = fromColumns


instance (f ~ context, g ~ Column context a) => K1Helper 'True context (SPINE a) g where
  type K1Columns 'True (SPINE a) g = HIdentity a
  toColumnsHelper = HIdentity
  fromColumnsHelper = unHIdentity


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


data HPairField x y a where
  HPairFst :: HField x a -> HPairField x y a
  HPairSnd :: HField y a -> HPairField x y a


instance (HigherKindedTable x, HigherKindedTable y) => HigherKindedTable (HPair x y) where
  type HConstrainTable (HPair x y) c = (HConstrainTable x c, HConstrainTable y c)
  type HField (HPair x y) = HPairField x y

  hfield (HPair l r) = \case
    HPairFst i -> hfield l i
    HPairSnd i -> hfield r i

  htabulate f = HPair (htabulate (f . HPairFst)) (htabulate (f . HPairSnd))

  hdicts = HPair hdicts hdicts

  htraverse f (HPair x y) = HPair <$> htraverse f x <*> htraverse f y


instance (Table f a, Table f b) => Table f (a, b) where
  type Columns (a, b) = HPair (Columns a) (Columns b)
  toColumns (a, b) = HPair (toColumns a) (toColumns b)
  fromColumns (HPair x y) = (fromColumns x, fromColumns y)


newtype HIdentity a f = HIdentity { unHIdentity :: Column f a }


data HIdentityField x y where
  HIdentityField :: HIdentityField x x


instance HigherKindedTable (HIdentity a) where
  type HConstrainTable (HIdentity a) c = (c a)
  type HField (HIdentity a) = HIdentityField a

  hfield (HIdentity a) HIdentityField = MkC a
  htabulate f = HIdentity $ toColumn $ f HIdentityField
  hdicts = HIdentity Dict

  htraverse :: forall f g m. Applicative m => (forall x. C f x -> m (C g x)) -> HIdentity a f -> m (HIdentity a g)
  htraverse f (HIdentity a) = HIdentity . toColumn @g <$> f (MkC a :: C f a)


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


instance (Table Expr (MaybeTable a), Table Expr a, ExprType (Maybe b) ~ MaybeTable a, ResultType a ~ b, SerializationMethod a b, HConstrainTable (Columns a) DBType) => SerializationMethod (MaybeTable a) (Maybe b) where
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
  MaybeTable t f <*> MaybeTable t' a = MaybeTable (liftNull (nullOr t t')) (f a)
    where
      nullOr x y =
        null_ (lit False) (\x' -> null_ (lit False) (x' ||.) y) x


instance Monad MaybeTable where
  MaybeTable t a >>= f = case f a of
    MaybeTable t' b -> MaybeTable (liftNull (nullOr t t')) b
    where
      nullOr x y =
        null_ (lit False) (\x' -> null_ (lit False) (x' ||.) y) x


data HMaybeTable g f =
  HMaybeTable
    { hnullTag :: Column f (Maybe Bool)
    , hcontents :: g f
    }
  deriving
    (Generic, HigherKindedTable)


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
class (ExprType a ~ Expr a, ResultType (Expr a) ~ a, ExprType (Maybe a) ~ Expr (Maybe a)) => DBType (a :: Type) where
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


null_ :: Expr b -> ( Expr a -> Expr b ) -> Expr ( Maybe a ) -> Expr b
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


mapTable
  :: (Columns s ~ Columns t, Table f s, Table g t)
  => (forall x. C f x -> C g x) -> s -> t
mapTable f = fromColumns . runIdentity . htraverse (pure . f) . toColumns


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



-- | The SQL @AND@ operator.
infixr 3 &&.
(&&.) :: Expr Bool -> Expr Bool -> Expr Bool
(&&.) ( Expr a ) ( Expr b ) =
    Expr ( Opaleye.BinExpr Opaleye.OpAnd a b )


-- | The SQL @NOT@ operator.
not_ :: Expr Bool -> Expr Bool
not_ ( Expr a ) =
  Expr ( Opaleye.UnExpr Opaleye.OpNot a )


-- | The @Function@ type class is an implementation detail that allows
-- @function@ to be polymorphic in the number of arguments it consumes.
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
function :: Function args result => String -> args -> result
function =
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


binaryOperator :: String -> Expr a -> Expr b -> Expr c
binaryOperator op (Expr a) (Expr b) =
  Expr $ Opaleye.BinExpr (Opaleye.OpOther op) a b


{-| Database types that can be compared for equality in queries.

Usually, this means producing an expression using the (overloaded) @=@
operator, but types can provide a more elaborate expression if necessary.

[ @DBEq@ with @newtype@s ]

Like with 'Rel8.DBType', @DBEq@ plays well with generalized newtype deriving.
The example given there showed a @UserId@ @newtype@, but we won't actually be
able to use that in joins or where-clauses because it lacks equality. We can
add this by changing our @newtype@ definition to:

@
newtype UserId = UserId { toInt32 :: Int32 }
  deriving ( DBType, DBEq )
@

This will re-use the equality logic for @Int32@, which is to just use the @=@
operator.

[ @DBEq@ with @DeriveAnyType@ ]

You can also use @DBEq@ with the @DeriveAnyType@ extension to easily add
equality to your type, assuming that @=@ is sufficient on @DBType@ encoded
values. Extending the example from 'Rel8.DBType', we could add equality
to @Color@ by writing:

@
data Color = Red | Green | Blue | Purple | Gold
  deriving ( Show, DBType, DBEq )
@

This means @Color@s will be treated as the literal strings @"Red"@, @"Green"@,
etc in the database, and they can be compared for equality by just using @=@.
-}
class DBType a => DBEq (a :: Type) where
  eqExprs :: Expr a -> Expr a -> Expr Bool
  eqExprs =
    binExpr (Opaleye.:==)


instance DBEq String


instance DBEq Int32


instance DBEq Int64


instance DBEq Text


instance DBEq Bool


instance DBEq a => DBEq ( Maybe a ) where
  eqExprs a b =
    null_ ( isNull b ) ( \a' -> null_ ( lit False ) ( eqExprs a' ) b ) a


-- | The type of @SELECT@able queries. You generally will not explicitly use
-- this type, instead preferring to be polymorphic over any 'MonadQuery m'.
-- Functions like 'select' will instantiate @m@ to be 'Query' when they run
-- queries.
newtype Query a = Query (Opaleye.Query a)
  deriving newtype (Functor, Applicative)


liftOpaleye :: Opaleye.Query a -> Query a
liftOpaleye =
  Query


toOpaleye :: Query a -> Opaleye.Query a
toOpaleye ( Query q ) =
  q


instance Monad Query where
  return = pure
  Query ( Opaleye.QueryArr f ) >>= g = Query $ Opaleye.QueryArr \input ->
    case f input of
      ( a, primQuery, tag ) ->
        case g a of
          Query ( Opaleye.QueryArr h ) ->
            h ( (), primQuery, tag )


-- | Run a @SELECT@ query, returning all rows.
select
  :: ( Serializable row haskell, MonadIO m )
  => Connection -> Query row -> m [ haskell ]
select = select_forAll


select_forAll
  :: forall row haskell m
   . ( Serializable row haskell, MonadIO m )
  => Connection -> Query row -> m [ haskell ]
select_forAll conn query =
  maybe
    ( return [] )
    ( liftIO . Database.PostgreSQL.Simple.queryWith_ ( queryParser query ) conn . fromString )
    ( selectQuery query )


queryParser
  :: Serializable sql haskell
  => Query sql
  -> Database.PostgreSQL.Simple.RowParser haskell
queryParser ( Query q ) =
  Opaleye.prepareRowParser
    queryRunner
    ( case Opaleye.runSimpleQueryArrStart q () of
        ( b, _, _ ) ->
          b
    )


queryRunner
  :: forall row haskell
   . Serializable row haskell
  => Opaleye.FromFields row haskell
queryRunner =
  Opaleye.QueryRunner ( void unpackspec ) (const rowParser) ( const 1 )


unpackspec :: Table Expr row => Opaleye.Unpackspec row row
unpackspec =
  Opaleye.Unpackspec $ Opaleye.PackMap \f -> 
    fmap fromColumns . htraverse (traverseC (traversePrimExpr f)) . toColumns


-- | Run an @INSERT@ statement
insert :: MonadIO m => Connection -> Insert result -> m result
insert connection Insert{ into, rows, onConflict, returning } =
  liftIO
    ( Opaleye.runInsert_
        connection
        ( toOpaleyeInsert into rows returning )
    )

  where

    toOpaleyeInsert
      :: forall schema result value
       . ( Table Expr value
         , Table ColumnSchema schema
         , Columns value ~ Columns schema
         )
      => TableSchema schema
      -> [ value ]
      -> Returning schema result
      -> Opaleye.Insert result
    toOpaleyeInsert into_ iRows returning_ =
      Opaleye.Insert
        { iTable = ddlTable into_ ( writer into_ )
        , iRows
        , iReturning = opaleyeReturning returning_
        , iOnConflict
        }

      where

        iOnConflict :: Maybe Opaleye.OnConflict
        iOnConflict =
          case onConflict of
            DoNothing ->
              Just Opaleye.DoNothing

            Abort ->
              Nothing


writer
  :: forall value schema
   . ( Table Expr value 
     , Table ColumnSchema schema
     , Columns value ~ Columns schema
     )
  => TableSchema schema -> Opaleye.Writer value schema
writer into_ =
  let
    go
      :: forall f list
       . ( Functor list, Applicative f )
      => ( ( list Opaleye.PrimExpr, String ) -> f () )
      -> list value
      -> f () 
    go f xs = 
      void $
        htraverse @(Columns schema) @(Compose f Expr) @Expr sequenceC $
          htabulate @(Columns schema) @(Compose f Expr) \i ->
            case hfield (toColumns (tableColumns into_)) i of
              MkC ColumnSchema{ columnName } ->
                MkC $ 
                  column columnName <$
                  f ( toPrimExpr . toColumn . flip hfield i . toColumns <$> xs
                    , columnName
                    )

  in
  Opaleye.Writer ( Opaleye.PackMap go )


opaleyeReturning :: Returning schema result -> Opaleye.Returning schema result
opaleyeReturning returning =
  case returning of
    NumberOfRowsInserted ->
      Opaleye.Count

    Projection f ->
      Opaleye.ReturningExplicit
        queryRunner
        ( f . mapTable ( mapC ( column . columnName ) ) )


ddlTable :: TableSchema schema -> Opaleye.Writer value schema -> Opaleye.Table value schema
ddlTable schema writer_ =
  toOpaleyeTable schema writer_ ( Opaleye.View ( tableColumns schema ) )


-- | The constituent parts of a SQL @INSERT@ statement.
data Insert :: * -> * where
  Insert
    :: (Columns value ~ Columns schema, Table Expr value, Table ColumnSchema schema)
    => { into :: TableSchema schema
         -- ^ Which table to insert into.
       , rows :: [ value ]
         -- ^ The rows to insert.
       , onConflict :: OnConflict
         -- ^ What to do if the inserted rows conflict with data already in the
         -- table.
       , returning :: Returning schema result
         -- ^ What information to return on completion.
       }
    -> Insert result


-- | @Returning@ describes what information to return when an @INSERT@
-- statement completes.
data Returning schema a where
  -- | Just return the number of rows inserted.
  NumberOfRowsInserted :: Returning schema Int64

  -- | Return a projection of the rows inserted. This can be useful if your
  -- insert statement increments sequences by using default values.
  --
  -- >>> :t insert Insert{ returning = Projection fooId }
  -- IO [ FooId ]
  Projection
    :: ( Table Expr projection
       , Table ColumnSchema schema
       , Table Expr row
       , Columns schema ~ Columns row
       , Serializable projection a
       )
    => ( row -> projection )
    -> Returning schema [ a ]


data OnConflict
  = Abort
  | DoNothing


selectQuery :: forall a . Table Expr a => Query a -> Maybe String
selectQuery ( Query opaleye ) =
  showSqlForPostgresExplicit

  where

    showSqlForPostgresExplicit =
      case Opaleye.runQueryArrUnpack unpackspec opaleye of
        ( x, y, z ) ->
          Opaleye.formatAndShowSQL
            ( x
            , Rel8.Optimize.optimize ( Opaleye.optimize y )
            , z
            )


delete :: MonadIO m => Connection -> Delete from returning -> m returning
delete c Delete{ from = deleteFrom, deleteWhere, returning } =
  liftIO ( Opaleye.runDelete_ c ( go deleteFrom deleteWhere returning ) )

  where

    go
      :: forall schema r row
       . ( Table Expr row 
         , Table ColumnSchema schema
         , Columns schema ~ Columns row
         )
      => TableSchema schema
      -> ( row -> Expr Bool )
      -> Returning schema r
      -> Opaleye.Delete r
    go schema deleteWhere_ returning_ =
      Opaleye.Delete
        { dTable = ddlTable schema ( Opaleye.Writer ( pure () ) )
        , dWhere =
            Opaleye.Column
              . toPrimExpr
              . deleteWhere_
              . mapTable (mapC (column . columnName))
        , dReturning = opaleyeReturning returning_
        }


data Delete from return where
  Delete
    :: ( Columns from ~ Columns row, Table Expr row, Table ColumnSchema from )
    => { from :: TableSchema from
       , deleteWhere :: row -> Expr Bool
       , returning :: Returning from return
       }
    -> Delete from return


update :: MonadIO m => Connection -> Update target returning -> m returning
update connection Update{ target, set, updateWhere, returning } =
  liftIO ( Opaleye.runUpdate_ connection ( go target set updateWhere returning ) )

  where

    go
      :: forall returning target row
       . ( Table Expr row
         , Columns target ~ Columns row
         , Table ColumnSchema target
         )
      => TableSchema target
      -> ( row -> row )
      -> ( row -> Expr Bool )
      -> Returning target returning
      -> Opaleye.Update returning
    go target_ set_ updateWhere_ returning_ =
      Opaleye.Update
        { uTable =
            ddlTable target_ (writer target_)

        , uReturning =
            opaleyeReturning returning_

        , uWhere =
            Opaleye.Column
              . toPrimExpr
              . updateWhere_
              . mapTable (mapC (column . columnName))

        , uUpdateWith =
            set_ . mapTable (mapC (column . columnName))
        }


data Update target returning where
  Update
    :: ( Columns target ~ Columns row, Table Expr row, Table ColumnSchema target )
    => { target :: TableSchema target
       , set :: row -> row
       , updateWhere :: row -> Expr Bool
       , returning :: Returning target returning
       }
    -> Update target returning


-- | Exists checks if a query returns at least one row.
--
-- @exists q@ is the same as the SQL expression @EXISTS ( q )@
exists :: Query a -> Query ( Expr Bool )
exists query =
  liftOpaleye ( lit True <$ Opaleye.restrictExists ( toOpaleye query ) )


-- | Select each row from a table definition.
--
-- This is equivalent to @FROM table@.
each :: (Columns schema ~ Columns row, Table Expr row, Table ColumnSchema schema) => TableSchema schema -> Query row
each = each_forAll


each_forAll
  :: forall schema row
   . ( Columns schema ~ Columns row, Table Expr row, Table ColumnSchema schema )
  => TableSchema schema -> Query row
each_forAll schema =
  liftOpaleye
    ( Opaleye.selectTableExplicit
        unpackspec
        ( toOpaleyeTable schema noWriter view )
    )

  where

    noWriter :: Opaleye.Writer () row
    noWriter =
      Opaleye.Writer ( Opaleye.PackMap \_ _ -> pure () )


    view :: Opaleye.View row
    view =
      Opaleye.View
        ( mapTable
            ( mapC ( column . columnName ) )
            ( tableColumns schema )
        )


-- | Select all rows from another table that match a given predicate. If the
-- predicate is not satisfied, a null 'MaybeTable' is returned.
--
-- @leftJoin t p@ is equivalent to @LEFT JOIN t ON p@.
optional :: Query a -> Query (MaybeTable a)
optional =
  liftOpaleye . Opaleye.laterally (Opaleye.QueryArr . go) . toOpaleye

  where

    go query (i, left, tag) =
      ( MaybeTable t' a, join, Opaleye.next tag' )

      where

        ( MaybeTable t a, right, tag' ) =
          Opaleye.runSimpleQueryArr (pure <$> query) (i, tag)

        ( t', bindings ) =
          Opaleye.run $
          Opaleye.runUnpackspec unpackspec (Opaleye.extractAttr "maybe" tag') t

        join =
          Opaleye.Join Opaleye.LeftJoin (toPrimExpr $ lit True) [] bindings left right


-- | Combine the results of two queries of the same type.
--
-- @union a b@ is the same as the SQL statement @x UNION b@.
union :: Table Expr a => Query a -> Query a -> Query a
union = union_forAll


union_forAll
  :: forall a
   . Table Expr a
  => Query a -> Query a -> Query a
union_forAll l r =
  liftOpaleye
    ( Opaleye.unionExplicit
        binaryspec
        ( toOpaleye l )
        ( toOpaleye r )
    )

  where

    binaryspec :: Opaleye.Binaryspec a a
    binaryspec =
      Opaleye.Binaryspec $ Opaleye.PackMap \f ( a, b ) ->
        zipTablesWithM
          ( zipCWithM \x y -> fromPrimExpr <$> f ( toPrimExpr x, toPrimExpr y ) )
          a
          b


-- | Select all distinct rows from a query, removing duplicates.
--
-- @distinct q@ is equivalent to the SQL statement @SELECT DISTINCT q@
distinct :: Table Expr a => Query a -> Query a
distinct = distinct_forAll


distinct_forAll :: forall a. Table Expr a => Query a -> Query a
distinct_forAll query =
  liftOpaleye ( Opaleye.distinctExplicit distinctspec ( toOpaleye query ) )

  where

    distinctspec :: Opaleye.Distinctspec a a
    distinctspec =
      Opaleye.Distinctspec $ Opaleye.Aggregator $ Opaleye.PackMap \f ->
        traverseTable (traverseC \x -> fromPrimExpr <$> f (Nothing, toPrimExpr x))


-- | @limit n@ select at most @n@ rows from a query.
--
-- @limit n@ is equivalent to the SQL @LIMIT n@.
limit :: Natural -> Query a -> Query a
limit n query =
  liftOpaleye
    ( Opaleye.limit
        ( fromIntegral n )
        ( toOpaleye query )
    )


-- | @offset n@ drops the first @n@ rows from a query.
--
-- @offset n@ is equivalent to the SQL @OFFSET n@.
offset :: Natural -> Query a -> Query a
offset n query =
  liftOpaleye
    ( Opaleye.offset
        ( fromIntegral n )
        ( toOpaleye query )
    )


-- | Drop any rows that don't match a predicate.
--
-- @where_ expr@ is equivalent to the SQL @WHERE expr@.
where_ :: Expr Bool -> Query ()
where_ x =
  liftOpaleye $ Opaleye.QueryArr \( (), left, t ) ->
    ( (), Opaleye.restrict ( toPrimExpr x ) left, t )


catMaybeTable :: MaybeTable a -> Query a
catMaybeTable MaybeTable{ nullTag, table } = do
  where_ $ not_ $ isNull nullTag
  return table


catMaybe :: Expr (Maybe a) -> Query (Expr a)
catMaybe e =
  catMaybeTable $ MaybeTable (ifThenElse_ (isNull e) (lit Nothing) (lit (Just False))) (unsafeCoerceExpr e)


values :: forall expr f. (Table Expr expr, Foldable f, HConstrainTable (Columns expr) DBType) => f expr -> Query expr
values = liftOpaleye . Opaleye.valuesExplicit valuesspec . toList
  where
    valuesspec = Opaleye.ValuesspecSafe packmap unpackspec
      where
        packmap :: Opaleye.PackMap Opaleye.PrimExpr Opaleye.PrimExpr () expr
        packmap = Opaleye.PackMap \f () -> 
          fmap fromColumns $
            htraverse (traverseC (traversePrimExpr f)) $
              htabulate @(Columns expr) @Expr \i -> 
                case hfield (hdicts @(Columns expr) @DBType) i of
                  MkC Dict -> MkC $ fromPrimExpr $ nullExpr i
            where
              nullExpr :: forall a w. DBType a => HField w a -> Opaleye.PrimExpr
              nullExpr _ = Opaleye.CastExpr typeName (Opaleye.ConstExpr Opaleye.NullLit)
                where
                  DatabaseType{ typeName } = typeInformation @a


filter :: (a -> Expr Bool) -> a -> Query a
filter f a = do
  where_ $ f a
  return a


-- | The class of database tables (containing one or more columns) that can be
-- compared for equality as a whole.
class Table Expr a => EqTable a where
  -- | Compare two tables or expressions for equality.
  --
  -- This operator is overloaded (much like Haskell's 'Eq' type class) to allow
  -- you to compare expressions:
  --
  -- >>> :t exprA
  -- Expr m Int
  --
  -- >>> :t exprA ==. exprA
  -- Expr m Bool
  --
  -- But you can also compare composite structures:
  --
  -- >>> :t ( exprA, exprA ) ==. ( exprA, exprA )
  -- Expr m Bool
  (==.) :: a -> a -> Expr Bool


-- | Any @Expr@s can be compared for equality as long as the underlying
-- database type supports equality comparisons.
instance DBEq a => EqTable (Expr a) where
  (==.) = eqExprs
