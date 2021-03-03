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

module Rel8
  ( -- * Database types
    DBType(..)

    -- ** Deriving-via helpers
  , JSONEncoded(..)
  , ReadShow(..)

    -- ** @DatabaseType@
  , DatabaseType(..)
  , mapDatabaseType
  , parseDatabaseType

    -- ** Database types with equality
  , DBEq(..)

    -- * Tables and higher-kinded tables
  , Table(..)
  , HigherKindedTable
  , Congruent

    -- ** Table schemas
  , Column
  , TableSchema(..)
  , ColumnSchema

    -- * Expressions
  , Expr
  , unsafeCoerceExpr
  , binaryOperator

    -- ** @null@
  , null_
  , isNull
  , liftNull
  , mapNull
  , liftOpNull
  , catMaybe

    -- ** Boolean operations
  , (&&.)
  , and_
  , (||.)
  , or_
  , not_
  , ifThenElse_
  , EqTable(..)

    -- ** Functions
  , Function
  , function
  , nullaryFunction

    -- * Queries
  , Query
  , showQuery

    -- ** Selecting rows
  , each
  , Selects
  , values

    -- ** Filtering
  , filter
  , where_
  , distinct

    -- ** @LIMIT@/@OFFSET@
  , limit
  , offset

    -- ** Combining 'Query's
  , union
  , exists

    -- ** Optional 'Query's
  , optional
  , MaybeTable
  , maybeTable
  , noTable
  , catMaybeTable

    -- * Aggregates
  , Aggregate
  , Foldability (Semifold, Fold)
  , aggregate
  , aggregate1
  , arrayAgg
  , arrayAgg1
  , groupBy
  , DBMax (max)

    -- * Compound aggregates
  , ListTable, many
  , NonEmptyTable, some

    -- ** Ordering
  , orderBy
  , Order
  , asc
  , desc
  , nullsFirst
  , nullsLast

    -- * IO
  , Serializable(..)

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
  ) where

-- aeson
import Data.Aeson ( FromJSON, ToJSON, Value, parseJSON, toJSON )
import Data.Aeson.Types ( parseEither )

-- base
import Control.Applicative ( ZipList(..), liftA2 )
import qualified Control.Applicative
import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Foldable ( Foldable( toList, foldl' ) )
import Data.Foldable ( fold )
import Data.Functor ( (<&>) )
import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Identity ( Identity( runIdentity ) )
import Data.Int ( Int32, Int64 )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty, nonEmpty )
import qualified Data.List.NonEmpty as NonEmpty
import Data.Proxy ( Proxy( Proxy ) )
import Data.String ( IsString(..) )
import Data.Typeable ( Typeable )
import GHC.Generics ( (:*:)(..), Generic, K1(..), M1(..), Rep, from, to )
import Numeric.Natural ( Natural )
import Prelude hiding ( filter, max )
import Text.Read ( readEither )

-- bytestring
import qualified Data.ByteString
import qualified Data.ByteString.Lazy

-- case-insensitive
import Data.CaseInsensitive ( CI )

-- opaleye
import qualified Opaleye ( Delete(..), Insert(..), OnConflict(..), Update(..), runDelete_, runInsert_, runUpdate_, valuesExplicit )
import qualified Opaleye.Aggregate as Opaleye
import qualified Opaleye.Binary as Opaleye
import qualified Opaleye.Distinct as Opaleye
import qualified Opaleye.Internal.Aggregate as Opaleye
import qualified Opaleye.Internal.Binary as Opaleye
import qualified Opaleye.Internal.Column as Opaleye
import qualified Opaleye.Internal.Distinct as Opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.Manipulation as Opaleye
import qualified Opaleye.Internal.Order as Opaleye
import qualified Opaleye.Internal.Optimize as Opaleye
import qualified Opaleye.Internal.PackMap as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye hiding ( BinOp, aggregate, limit )
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
  , pgTimeOfDay
  , pgUTCTime
  , pgUUID
  , pgValueJSON
  , pgZonedTime
  )
import qualified Opaleye.Table as Opaleye

-- postgresql-simple
import qualified Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple ( Connection )
import Database.PostgreSQL.Simple.FromField
  ( FieldParser
  , FromField
  , ResultError( Incompatible )
  , fromField
  , optionalField
  , pgArrayFieldParser
  , returnError
  )
import Database.PostgreSQL.Simple.FromRow ( RowParser, fieldWith )
import qualified Database.PostgreSQL.Simple.FromRow as Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types ( PGArray( PGArray, fromPGArray ) )

-- rel8
import qualified Rel8.Optimize

-- scientific
import Data.Scientific ( Scientific )

-- text
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.Lazy

-- time
import Data.Time ( Day, LocalTime, TimeOfDay, UTCTime, ZonedTime )

-- uuid
import Data.UUID ( UUID )
import Data.Functor.Contravariant (Contravariant)
import Data.Functor.Contravariant.Divisible (Divisible, Decidable)
import Data.Functor.Const (Const(Const), getConst)
import Data.Bifunctor (first)


{-| Haskell types that can be represented as expressions in a database. There
should be an instance of @DBType@ for all column types in your database schema
(e.g., @int@, @timestamptz@, etc).

Rel8 comes with stock instances for all default types in PostgreSQL, so you
should only need to derive instances of this class for custom database types,
such as types defined in PostgreSQL extensions, or custom domain types.

[ Creating @DBType@s using @newtype@ ]

Generalized newtype deriving can be used when you want use a @newtype@ around a
database type for clarity and accuracy in your Haskell code. A common example is
to @newtype@ row id types:

@
newtype UserId = UserId { toInt32 :: Int32 }
  deriving (DBType)
@

You can now write queries using @UserId@ instead of @Int32@, which may help
avoid making bad joins. However, when SQL is generated, it will be as if you
just used integers (the type distinction does not impact query generation).
-}
class (AnExpr a, Typeable a) => DBType (a :: Type) where
  -- | Lookup the type information for the type @a@.
  typeInformation :: DatabaseType a


{-| A deriving-via helper type for column types that store a Haskell value
using a JSON encoding described by @aeson@'s 'ToJSON' and 'FromJSON' type
classes.

The declaration:

@
data Pet = Pet { petName :: String, petAge :: Int }
  deriving (Generic, ToJSON, FromJSON)
  deriving DBType via JSONEncoded Pet
@

will allow you to store @Pet@ values in a single SQL column (stored as @json@
values).
-}
newtype JSONEncoded a = JSONEncoded { fromJSONEncoded :: a }


instance (FromJSON a, ToJSON a, Typeable a) => DBType (JSONEncoded a) where
  typeInformation = parseDatabaseType f g typeInformation
    where
      f = fmap JSONEncoded . parseEither parseJSON
      g = toJSON . fromJSONEncoded


-- | A deriving-via helper type for column types that store a Haskell value
-- using a Haskell's 'Read' and 'Show' type classes.
newtype ReadShow a = ReadShow { fromReadShow :: a }


{-| Anything that has an instance of 'DBType' is an 'Expr'. This class packages that knowledge up. -}
class
  ( ExprType a ~ Expr a
  , ResultType (Expr a) ~ a
  , ExprType (Maybe a) ~ Expr (Maybe a)
  , ExprType [a] ~ Expr [a]
  , ExprType (NonEmpty a) ~ Expr (NonEmpty a)
  ) => AnExpr (a :: Type)


instance
  ( ExprType a ~ Expr a
  , ResultType (Expr a) ~ a
  , ExprType (Maybe a) ~ Expr (Maybe a)
  , ExprType [a] ~ Expr [a]
  , ExprType (NonEmpty a) ~ Expr (NonEmpty a)
  ) => AnExpr a


{-| A @DatabaseType@ describes how to encode and decode a Haskell type to and
from database queries. The @typeName@ is the name of the type in the database,
which is used to accurately type literals. 
-}
data DatabaseType (a :: Type) = DatabaseType
  { encode :: a -> Opaleye.PrimExpr
    -- ^ How to encode a single Haskell value as a SQL expression.
  , decode :: FieldParser a
    -- ^ How to deserialize a single result back to Haskell.
  , typeName :: String
    -- ^ The name of the SQL type.
  }


{-| Simultaneously map over how a type is both encoded and decoded, while
retaining the name of the type. This operation is useful if you want to
essentially @newtype@ another 'DatabaseType'. 
-}
mapDatabaseType :: (a -> b) -> (b -> a) -> DatabaseType a -> DatabaseType b
mapDatabaseType aToB bToA DatabaseType{ encode, decode, typeName } = DatabaseType
  { encode = encode . bToA
  , decode = \x y -> aToB <$> decode x y
  , typeName
  }


{-| Apply a parser to a 'DatabaseType'.

This can be used if the data stored in the database should only be subset of a
given 'DatabaseType'. The parser is applied when deserializing rows returned -
the encoder assumes that the input data is already in the appropriate form.
-}
parseDatabaseType :: Typeable b => (a -> Either String b) -> (b -> a) -> DatabaseType a -> DatabaseType b
parseDatabaseType aToB bToA DatabaseType{ encode, decode, typeName } = DatabaseType
  { encode = encode . bToA
  , decode = \x y -> decode x y >>= either (returnError Incompatible x) return . aToB
  , typeName
  }


{-| Database column types that can be compared for equality in queries.

Usually, this means producing an expression using the (overloaded) @=@
operator, but types can provide a more elaborate expression if necessary.

[ @DBEq@ with @newtype@s ]

Like with 'Rel8.DBType', @DBEq@ plays well with generalized newtype deriving.
The example given for @DBType@ added a @UserId@ @newtype@, but without a @DBEq@
instance won't actually be able to use that in joins or where-clauses, because
it lacks equality. We can add this by changing our @newtype@ definition to:

@
newtype UserId = UserId { toInt32 :: Int32 }
  deriving (DBType, DBEq)
@

This will re-use the equality logic for @Int32@, which is to just use the @=@
operator.

[ @DBEq@ with @DeriveAnyType@ ]

You can also use @DBEq@ with the @DeriveAnyType@ extension to easily add
equality to your type, assuming that @=@ is sufficient on @DBType@ encoded
values. Extending the example from 'Rel8.ReadShow''s 'Rel8.DBType' instance, we
could add equality to @Color@ by writing:

@
data Color = Red | Green | Blue | Purple | Gold
  deriving (Generic, Show, Read, DBEq)
  deriving DBType via ReadShow Color
@

This means @Color@s will be treated as the literal strings @"Red"@, @"Green"@,
etc, in the database, and they can be compared for equality by just using @=@.
-}
class DBType a => DBEq (a :: Type) where
  eqExprs :: Expr a -> Expr a -> Expr Bool
  eqExprs = binExpr (Opaleye.:==)


-- | Typed SQL expressions
newtype Expr (a :: Type) = Expr { toPrimExpr :: Opaleye.PrimExpr }


-- | Unsafely treat an 'Expr' that returns @a@s as returning @b@s.
unsafeCoerceExpr :: Expr a -> Expr b
unsafeCoerceExpr (Expr x) = Expr x


-- | Construct an expression by applying an infix binary operator to two
-- operands.
binaryOperator :: String -> Expr a -> Expr b -> Expr c
binaryOperator op (Expr a) (Expr b) = Expr $ Opaleye.BinExpr (Opaleye.OpOther op) a b


-- | Like 'maybe', but to eliminate @null@.
null_ :: DBType b => Expr b -> (Expr a -> Expr b) -> Expr (Maybe a) -> Expr b
null_ whenNull f a = ifThenElse_ (isNull a) whenNull (f (retype a))


-- | Like 'isNothing', but for @null@.
isNull :: Expr (Maybe a) -> Expr Bool
isNull = fromPrimExpr . Opaleye.UnExpr Opaleye.OpIsNull . toPrimExpr


{-| Lift an expression that's not null to a type that might be @null@. This is
an identity operation in terms of any generated query, and just modifies the
query's type.
-}
liftNull :: Expr a -> Expr ( Maybe a )
liftNull = retype


mapNull :: (Expr a -> Expr b) -> Expr (Maybe a) -> Expr (Maybe b)
mapNull f = retype . f . retype


liftOpNull :: (Expr a -> Expr b -> Expr c) -> Expr (Maybe a) -> Expr (Maybe b) -> Expr (Maybe c)
liftOpNull f a b = retype (f (retype a) (retype b))


{-| Filter a 'Query' that might return @null@ to a 'Query' without any @null@s.

Corresponds to 'catMaybes'.
-}
catMaybe :: Expr (Maybe a) -> Query (Expr a)
catMaybe e = catMaybeTable $ MaybeTable nullTag (unsafeCoerceExpr e)
  where
    nullTag = ifThenElse_ (isNull e) (lit Nothing) (lit (Just False))


-- | The SQL @AND@ operator.
infixr 3 &&.


(&&.) :: Expr Bool -> Expr Bool -> Expr Bool
Expr a &&. Expr b = Expr $ Opaleye.BinExpr Opaleye.OpAnd a b


{-| Fold @AND@ over a collection of expressions.
 
@and_ mempty = lit True@
-}
and_ :: Foldable f => f (Expr Bool) -> Expr Bool
and_ = foldl' (&&.) (lit True)


-- | The SQL @OR@ operator.
infixr 2 ||.


(||.) :: Expr Bool -> Expr Bool -> Expr Bool
Expr a ||. Expr b = Expr $ Opaleye.BinExpr Opaleye.OpOr a b


{-| Fold @OR@ over a collection of expressions.
 
@or_ mempty = lit False@
-}
or_ :: Foldable f => f (Expr Bool) -> Expr Bool
or_ = foldl' (||.) (lit False)


-- | The SQL @NOT@ operator.
not_ :: Expr Bool -> Expr Bool
not_ (Expr a) = Expr $ Opaleye.UnExpr Opaleye.OpNot a


{-| Branch two expressions based on a predicate. Similar to @if ... then ...
else@ in Haskell (and implemented using @CASE@ in SQL).
-}
ifThenElse_ :: Table Expr a => Expr Bool -> a -> a -> a
ifThenElse_ bool whenTrue = case_ [(bool, whenTrue)]


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


-- | The @Function@ type class is an implementation detail that allows
-- @function@ to be polymorphic in the number of arguments it consumes.
class Function arg res where
  -- | Build a function of multiple arguments.
  applyArgument :: ([Opaleye.PrimExpr] -> Opaleye.PrimExpr) -> arg -> res


instance arg ~ Expr a => Function arg (Expr res) where
  applyArgument mkExpr (Expr a) = Expr $ mkExpr [a]


instance (arg ~ Expr a, Function args res) => Function arg (args -> res) where
  applyArgument f (Expr a) = applyArgument (f . (a :))


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
function = applyArgument . Opaleye.FunExpr


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
  where
    nullaryFunction_forAll :: forall a. DBType a => String -> Expr a
    nullaryFunction_forAll name =
      const (Expr (Opaleye.FunExpr name [])) (lit (undefined :: a))


{-| Types that represent SQL tables.

You generally should not need to derive instances of this class manually, as
writing higher-kinded data types is usually more convenient. See also:
'HigherKindedTable'.

-}
class HigherKindedTable (Columns t) => Table (context :: Type -> Type) (t :: Type) | t -> context where
  type Columns t :: (Type -> Type) -> Type

  toColumns :: t -> Columns t context
  fromColumns :: Columns t context -> t


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
class HConstrainTable t DBType => HigherKindedTable (t :: (Type -> Type) -> Type) where
  type HField t = (field :: Type -> Type) | field -> t
  type HConstrainTable t (c :: Type -> Constraint) :: Constraint

  hfield :: t f -> HField t x -> C f x
  htabulate :: forall f. (forall x. HField t x -> C f x) -> t f
  htraverse :: forall f g m. Applicative m => (forall x. C f x -> m (C g x)) -> t f -> m (t g)
  hdicts :: forall c. HConstrainTable t c => t (Dict c)

  type HField t = GenericHField t
  type HConstrainTable t c = HConstrainTable (Columns (WithShape IsColumn (Rep (t IsColumn)) (Rep (t IsColumn) ()))) c

  default hfield
    :: forall f x
     . ( Generic (t f)
       , HField t ~ GenericHField t
       , Congruent (WithShape f (Rep (t IsColumn)) (Rep (t f) ())) (WithShape IsColumn (Rep (t IsColumn)) (Rep (t f) ()))
       , HField (Columns (WithShape IsColumn (Rep (t IsColumn)) (Rep (t f) ()))) ~ HField (Columns (WithShape IsColumn (Rep (t IsColumn)) (Rep (t IsColumn) ())))
       , HigherKindedTable (Columns (WithShape IsColumn (Rep (t IsColumn)) (Rep (t f) ())))
       , Table f (WithShape f (Rep (t IsColumn)) (Rep (t f) ()))
       )
    => t f -> HField t x -> C f x
  hfield x (GenericHField i) =
    hfield (toColumns (WithShape @f @(Rep (t IsColumn)) (GHC.Generics.from @_ @() x))) i

  default htabulate
    :: forall f
     . ( Generic (t f)
       , HField t ~ GenericHField t
       , Congruent (WithShape f (Rep (t IsColumn)) (Rep (t f) ())) (WithShape IsColumn (Rep (t IsColumn)) (Rep (t f) ()))
       , HField (Columns (WithShape IsColumn (Rep (t IsColumn)) (Rep (t f) ()))) ~ HField (Columns (WithShape IsColumn (Rep (t IsColumn)) (Rep (t IsColumn) ())))
       , HigherKindedTable (Columns (WithShape IsColumn (Rep (t IsColumn)) (Rep (t f) ())))
       , Table f (WithShape f (Rep (t IsColumn)) (Rep (t f) ()))
       )
    => (forall a. HField t a -> C f a) -> t f
  htabulate f =
    to @_ @() $ forgetShape @f @(Rep (t IsColumn)) $ fromColumns $ htabulate (f . GenericHField)

  default htraverse
    :: forall f g m
     . ( Applicative m
       , Generic (t f)
       , Generic (t g)
       , Congruent (WithShape f (Rep (t IsColumn)) (Rep (t f) ())) (WithShape IsColumn (Rep (t IsColumn)) (Rep (t f) ()))
       , HigherKindedTable (Columns (WithShape IsColumn (Rep (t IsColumn)) (Rep (t f) ())))
       , Table f (WithShape f (Rep (t IsColumn)) (Rep (t f) ()))
       , Table g (WithShape g (Rep (t IsColumn)) (Rep (t g) ()))
       , Congruent (WithShape g (Rep (t IsColumn)) (Rep (t g) ())) (WithShape IsColumn (Rep (t IsColumn)) (Rep (t f) ()))
       )
    => (forall a. C f a -> m (C g a)) -> t f -> m (t g)
  htraverse f x =
    fmap (to @_ @() . forgetShape @g @(Rep (t IsColumn)) . fromColumns)
      $ htraverse f
      $ toColumns
      $ WithShape @f @(Rep (t IsColumn))
      $ GHC.Generics.from @_ @() x

  default hdicts
    :: forall c
     . ( Generic (t (Dict c))
       , Table (Dict c) (WithShape (Dict c) (Rep (t IsColumn)) (Rep (t (Dict c)) ()))
       , HConstrainTable (Columns (WithShape (Dict c) (Rep (t IsColumn)) (Rep (t (Dict c)) ()))) c
       )
    => t (Dict c)
  hdicts =
    to @_ @() $
      forgetShape @(Dict c) @(Rep (t IsColumn)) $
        fromColumns $
          hdicts @(Columns (WithShape (Dict c) (Rep (t IsColumn)) (Rep (t (Dict c)) ()))) @c


{-| The schema for a table. This is used to specify the name and schema
that a table belongs to (the @FROM@ part of a SQL query), along with
the schema of the columns within this table.

For each selectable table in your database, you should provide a @TableSchema@
in order to interact with the table via Rel8. For a table storing a list of
Haskell packages (as defined in the example for 'Rel8.Column.Column'), we would
write:

@
haskellPackage :: TableSchema ( HaskellPackage 'Rel8.ColumnSchema.ColumnSchema' )
haskellPackage =
  TableSchema
    { tableName = "haskell_package"
    , tableSchema = Nothing -- Assumes that haskell_package is reachable from your connections search_path
    , tableColumns =
        HaskellPackage { packageName = "name"
                       , packageAuthor = "author"
                       }
    }
@
-}
data TableSchema (schema :: Type) = TableSchema
  { tableName :: String
    -- ^ The name of the table.
  , tableSchema :: Maybe String
    -- ^ The schema that this table belongs to. If 'Nothing', whatever is on
    -- the connection's @search_path@ will be used.
  , tableColumns :: schema
    -- ^ The columns of the table. Typically you would use a a higher-kinded
    -- data type here, parameterized by the 'Rel8.ColumnSchema.ColumnSchema' functor.
  } deriving stock Functor


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

In queries, @f@ will be some type of 'Expr', and @Column Expr a@
reduces to just @Expr a@:

>>> :t packageName (package :: Package Expr)
Expr String

When we 'select' queries of this type, @f@ will be instantiated as
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
traverseC :: Applicative m => (Column f x -> m (Column g y)) -> C f x -> m (C g y)
traverseC f (MkC x) = MkC <$> f x


-- | If a column contains an effectful operation, sequence that operation into a
--  new column.
sequenceC :: (Column f a ~ m (Column g y), Functor m) => C f a -> m (C g y)
sequenceC (MkC x) = MkC <$> x


-- | Zip two columns together under an effectful context.
zipCWithM :: Applicative m => (Column f x -> Column g y -> m (Column h z)) -> C f x -> C g y -> m (C h z)
zipCWithM f (MkC x) (MkC y) = MkC <$> f x y


{-| To facilitate generic deriving for higher-kinded table, we work through
Tables and the WithShape wrapper. The idea is that whenever we have a 't f', we
can view this as a specific Table instance for Rep (t f). However, the story is
not quite as simple as a typical generic traversal. For higher kinded tables,
we expect one of two things to be true for all fields:

1. The field is a Column application. In this case we know that we've got a
   single DBType, and we need to have a single HIdentity in Columns.

2. The field is a nested Table. In this case, we need to concatenate all
   Columns of this Table into the parent Table.

To distinguish between these two cases, we apply t to a special IsColumn tag.
This controlled application lets us observe more information at each K1 node in
the rep.

However, there's /another/ complication! If we have 't Identity', then any
Column fields will vanish, but we'll be unable to easily see this in the K1
node. To deal with this, we also explicitly track the context in the
'WithShape' type.
-}
newtype WithShape (context :: Type -> Type) (shape :: Type -> Type) a = WithShape { forgetShape :: a }


-- | A special functor for use with Column to see the structure of a
-- higher-kinded table.
data IsColumn a


{-| We would like to write a default type

@
type HField t = HField (Columns (Rep ..))
@

but this will violate the injectivity of the HField type (as there might be
two 't's with the same 'Rep'). This newtype restores that injectivity.
-}
newtype GenericHField t a where
  GenericHField :: HField (Columns (WithShape IsColumn (Rep (t IsColumn)) (Rep (t IsColumn) ()))) a -> GenericHField t a


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
  IsColumnApplication (IsColumn _) = 'True
  IsColumnApplication _            = 'False


{-| This helper lets us distinguish between 'fieldN :: Column f Int' and
'nestedTable :: t f' fields in higher kinded tables. 
-}
class (isColumnApplication ~ IsColumnApplication shape, HigherKindedTable (K1Columns isColumnApplication shape a)) => K1Helper (isColumnApplication :: Bool) (context :: Type -> Type) (shape :: Type) (a :: Type) where
  type K1Columns isColumnApplication shape a :: (Type -> Type) -> Type
  toColumnsHelper :: a -> K1Columns isColumnApplication shape a context
  fromColumnsHelper :: K1Columns isColumnApplication shape a context -> a


instance (Table context a, IsColumnApplication shape ~ 'False) => K1Helper 'False context shape a where
  type K1Columns 'False shape a = Columns a
  toColumnsHelper = toColumns
  fromColumnsHelper = fromColumns


instance (DBType a, f ~ context, g ~ Column context a) => K1Helper 'True context (IsColumn a) g where
  type K1Columns 'True (IsColumn a) g = HIdentity a
  toColumnsHelper = HIdentity
  fromColumnsHelper = unHIdentity


-- | Any 'HigherKindedTable' is also a 'Table'.
instance (HigherKindedTable t, f ~ g) => Table f (t g) where
  type Columns (t g) = t
  toColumns = id
  fromColumns = id


{-| Pair two higher-kinded tables. This is primarily used to facilitate generic
deriving of higher-kinded tables with more than 1 field (it deals with the
@:*:@ case).
-}
data HPair x y (f :: Type -> Type) = HPair { hfst :: x f, hsnd :: y f }
  deriving stock (Generic)


-- | A HField type for indexing into HPair.
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


{-| A single-column higher-kinded table. This is primarily useful for
facilitating generic-deriving of higher kinded tables.
-}
newtype HIdentity a f = HIdentity { unHIdentity :: Column f a }


data HIdentityField x y where
  HIdentityField :: HIdentityField x x


instance DBType a => HigherKindedTable (HIdentity a) where
  type HConstrainTable (HIdentity a) c = (c a)
  type HField (HIdentity a) = HIdentityField a

  hfield (HIdentity a) HIdentityField = MkC a
  htabulate f = HIdentity $ toColumn $ f HIdentityField
  hdicts = HIdentity Dict

  htraverse :: forall f g m. Applicative m => (forall x. C f x -> m (C g x)) -> HIdentity a f -> m (HIdentity a g)
  htraverse f (HIdentity a) = HIdentity . toColumn @g <$> f (MkC a :: C f a)


{-| @Serializable@ witnesses the one-to-one correspondence between the type
@sql@, which contains SQL expressions, and the type @haskell@, which contains
the Haskell decoding of rows containing @sql@ SQL expressions.
-}
class (Table Expr expr, expr ~ ExprType haskell, haskell ~ ResultType expr) => Serializable expr haskell where
  lit :: haskell -> expr

  -- TODO Don't use Applicative f, instead supply a htraverse function. We _don't_ want access to 'pure'
  rowParser :: forall f. Applicative f
    => (forall x. Typeable x => FieldParser x -> FieldParser (f x))
    -> RowParser (f haskell)


-- | Compute the corresponding expression type for a Haskell response type.
type family ExprType (a :: Type) :: Type where
  ExprType (a, b) = (ExprType a, ExprType b)
  ExprType (t Identity) = t Expr
  ExprType (Maybe (t Identity)) = MaybeTable (t Expr)
  ExprType (Maybe (a, b)) = MaybeTable (ExprType (a, b))
  ExprType (Maybe a) = Expr (Maybe a)
  ExprType [t Identity] = ListTable Expr (t Expr)
  ExprType [(a, b)] = ListTable Expr (ExprType (a, b))
  ExprType [a] = Expr [a]
  ExprType (NonEmpty (t Identity)) = NonEmptyTable Expr (t Expr)
  ExprType (NonEmpty (a, b)) = NonEmptyTable Expr (ExprType (a, b))
  ExprType (NonEmpty a) = Expr (NonEmpty a)
  ExprType a = Expr a


-- | Compute the corresponding expression type for a SQL response type.
type family ResultType (a :: Type) :: Type where
  ResultType (a, b) = (ResultType a, ResultType b)
  ResultType (t Expr) = t Identity
  ResultType (Expr a) = a
  ResultType (MaybeTable a) = Maybe (ResultType a)
  ResultType (ListTable Expr a) = [ResultType a]
  ResultType (NonEmptyTable Expr a) = NonEmpty (ResultType a)


-- | Any higher-kinded records can be @SELECT@ed, as long as we know how to
-- decode all of the records constituent part's.
instance (s ~ t, expr ~ Expr, identity ~ Identity, HigherKindedTable t, HConstrainTable t DBType) => Serializable (s expr) (t identity) where
  rowParser :: forall f. Applicative f => (forall a. Typeable a => FieldParser a -> FieldParser (f a)) -> RowParser (f (t identity))
  rowParser inject = getCompose $ htraverse sequenceC $ htabulate (f . hfield (hdicts @t @DBType))
    where
      f :: forall a. C (Dict DBType) a -> C (Compose (Compose RowParser f) Identity) a
      f (MkC Dict) = MkC $ Compose $ fieldWith $ inject $ decode $ typeInformation @a

  lit t =
    fromColumns $ htabulate \i ->
      case (hfield (hdicts @t @DBType) i, hfield t i) of
        (MkC Dict, MkC x) -> MkC $ lit x


instance (DBType a, a ~ b) => Serializable (Expr a) b where
  rowParser inject = fieldWith $ inject $ decode typeInformation

  lit = Expr . Opaleye.CastExpr typeName . encode
    where
      DatabaseType{ encode, typeName } = typeInformation


instance (Serializable a1 b1, Serializable a2 b2) => Serializable (a1, a2) (b1, b2) where
  rowParser inject = liftA2 (,) <$> rowParser @a1 inject <*> rowParser @a2 inject

  lit (a, b) = (lit a, lit b)


instance (ExprType (Maybe b) ~ MaybeTable a, Serializable a b) => Serializable (MaybeTable a) (Maybe b) where
  rowParser inject = do
    tags <- fieldWith $ inject $ decode typeInformation
    rows <- rowParser @a \fieldParser x y -> Compose <$> inject (fallback fieldParser) x y
    return $ liftA2 f tags (getCompose rows)
    where
      f :: Maybe Bool -> Maybe b -> Maybe b
      f (Just True)  (Just row) = Just row
      f (Just True)  Nothing    = error "TODO"
      f _            _          = Nothing

      fallback :: forall x. FieldParser x -> FieldParser (Maybe x)
      fallback fieldParser x (Just y) = Just <$> fieldParser x (Just y)
      fallback fieldParser x Nothing = Control.Applicative.optional (fieldParser x Nothing)

  lit = \case
    Nothing -> noTable
    Just x  -> pure $ lit x


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
  deriving stock Functor


instance Applicative MaybeTable where
  pure = MaybeTable (lit (Just True))
  MaybeTable t f <*> MaybeTable t' a = MaybeTable (liftOpNull (&&.) t t') (f a)


instance Monad MaybeTable where
  MaybeTable t a >>= f = case f a of
    MaybeTable t' b -> MaybeTable (liftOpNull (&&.) t t') b


data HMaybeTable g f = HMaybeTable
  { hnullTag :: Column f (Maybe Bool)
  , htable :: g f
  }
  deriving stock Generic
  deriving anyclass HigherKindedTable


instance Table Expr a => Table Expr (MaybeTable a) where
  type Columns (MaybeTable a) = HMaybeTable (Columns a)

  toColumns (MaybeTable x y) = HMaybeTable x (toColumns y)
  fromColumns (HMaybeTable x y) = MaybeTable x (fromColumns y)


-- | Perform case analysis on a 'MaybeTable'. Like 'maybe'.
maybeTable
  :: Table Expr b
  => b -> (a -> b) -> MaybeTable a -> b
maybeTable def f MaybeTable{ nullTag, table } =
  ifThenElse_ (null_ (lit False) id nullTag) (f table) def


-- | The null table. Like 'Nothing'.
noTable :: forall a. Table Expr a => MaybeTable a
noTable = MaybeTable (lit Nothing) $ fromColumns $ htabulate f
  where
    f :: forall x. HField (Columns a) x -> C Expr x
    f i =
      case hfield (hdicts @(Columns a) @DBType) i of
        MkC Dict -> MkC $ unsafeCoerceExpr (lit (Nothing :: Maybe x))


instance (DBType a, expr ~ Expr) => Table expr (Expr a) where
  type Columns (Expr a) = HIdentity a
  toColumns = HIdentity
  fromColumns = unHIdentity


fromOpaleye :: forall a b. (FromField a, IsSqlType b) => (a -> Opaleye.Column b) -> DatabaseType a
fromOpaleye f =
  DatabaseType
    { encode = \x -> case f x of Opaleye.Column e -> e
    , decode = fromField
    , typeName = showSqlType (Proxy @b)
    }


-- | Corresponds to the @bool@ PostgreSQL type.
instance DBType Bool where
  typeInformation = fromOpaleye pgBool


-- | Corresponds to the @int4@ PostgreSQL type.
instance DBType Int32 where
  typeInformation = mapDatabaseType fromIntegral fromIntegral $ fromOpaleye pgInt4


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


instance DBType a => DBType [a] where
  typeInformation = DatabaseType
    { encode = Opaleye.ArrayExpr . map encode
    , decode = fmap (fmap fromPGArray) <$> pgArrayFieldParser decode
    , typeName = typeName <> "[]"
    }
    where
      DatabaseType{ encode, decode, typeName } = typeInformation


instance DBType a => DBType (NonEmpty a) where
  typeInformation = parseDatabaseType nonEmptyEither toList typeInformation
    where
      nonEmptyEither =
        maybe (Left "DBType.NonEmpty.decode: empty list") Right . nonEmpty


case_ :: forall a. Table Expr a => [ ( Expr Bool, a ) ] -> a -> a
case_ alts def =
  fromColumns $ htabulate @(Columns a) \x -> MkC $ fromPrimExpr $
    Opaleye.CaseExpr
        [ ( toPrimExpr bool, toPrimExpr $ toColumn $ hfield (toColumns alt) x ) | ( bool, alt ) <- alts ]
        ( toPrimExpr $ toColumn $ hfield (toColumns def) x )


retype :: forall b a. Expr a -> Expr b
retype = fromPrimExpr . toPrimExpr


fromPrimExpr :: Opaleye.PrimExpr -> Expr a
fromPrimExpr = Expr


{-| The 'DBType' instance for 'ReadShow' allows you to serialize a type using
Haskell's 'Read' and 'Show' instances:

@
data Color = Red | Green | Blue
  deriving (Read, Show)
  deriving DBType via ReadShow Color
@
-}
instance (Read a, Show a, Typeable a) => DBType (ReadShow a) where
  typeInformation =
    parseDatabaseType (fmap ReadShow . readEither . Text.unpack) (Text.pack . show . fromReadShow) typeInformation


mapTable
  :: (Congruent s t, Table f s, Table g t)
  => (forall x. C f x -> C g x) -> s -> t
mapTable f = fromColumns . runIdentity . htraverse (pure . f) . toColumns


zipTablesWithM
  :: forall x y z f g h m
   . (Congruent x y, Columns y ~ Columns z, Table f x, Table g y, Table h z, Applicative m)
  => (forall a. C f a -> C g a -> m (C h a)) -> x -> y -> m z
zipTablesWithM f (toColumns -> x) (toColumns -> y) =
  fmap fromColumns $
    htraverse sequenceC $
      htabulate @_ @(Compose m h) $
        MkC . fmap toColumn . liftA2 f (hfield x) (hfield y)


traverseTable
  :: (Congruent x y, Table f x, Table g y, Applicative m)
  => (forall a. C f a -> m (C g a)) -> x -> m y
traverseTable f = fmap fromColumns . htraverse f . toColumns


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
liftOpaleye = Query


toOpaleye :: Query a -> Opaleye.Query a
toOpaleye (Query q) = q


mapOpaleye :: (Opaleye.Query a -> Opaleye.Query b) -> Query a -> Query b
mapOpaleye f = liftOpaleye . f . toOpaleye


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
queryRunner = Opaleye.QueryRunner (void unpackspec) (const (runIdentity <$> rowParser (\f x y -> pure <$> f x y))) (const 1)


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
       . Selects schema value
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
   . Selects schema value
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
  toOpaleyeTable schema writer_ $ Opaleye.View (tableColumns schema)


-- | The constituent parts of a SQL @INSERT@ statement.
data Insert :: Type -> Type where
  Insert
    :: Selects schema value
    => { into :: TableSchema schema
         -- ^ Which table to insert into.
       , rows :: [value]
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
    :: ( Selects schema row, Serializable projection a )
    => (row -> projection)
    -> Returning schema [a]


data OnConflict
  = Abort
  | DoNothing


selectQuery :: forall a . Table Expr a => Query a -> Maybe String
selectQuery (Query opaleye) = showSqlForPostgresExplicit
  where
    showSqlForPostgresExplicit =
      case Opaleye.runQueryArrUnpack unpackspec opaleye of
        (x, y, z) -> Opaleye.formatAndShowSQL (x , Rel8.Optimize.optimize (Opaleye.optimize y) , z)


delete :: MonadIO m => Connection -> Delete from returning -> m returning
delete c Delete{ from = deleteFrom, deleteWhere, returning } =
  liftIO $ Opaleye.runDelete_ c $ go deleteFrom deleteWhere returning

  where

    go
      :: forall schema r row
       . Selects schema row 
      => TableSchema schema
      -> (row -> Expr Bool)
      -> Returning schema r
      -> Opaleye.Delete r
    go schema deleteWhere_ returning_ =
      Opaleye.Delete
        { dTable = ddlTable schema $ Opaleye.Writer $ pure ()
        , dWhere =
            Opaleye.Column
              . toPrimExpr
              . deleteWhere_
              . mapTable (mapC (column . columnName))
        , dReturning = opaleyeReturning returning_
        }


data Delete from return where
  Delete
    :: Selects from row
    => { from :: TableSchema from
       , deleteWhere :: row -> Expr Bool
       , returning :: Returning from return
       }
    -> Delete from return


update :: MonadIO m => Connection -> Update target returning -> m returning
update connection Update{ target, set, updateWhere, returning } =
  liftIO $ Opaleye.runUpdate_ connection (go target set updateWhere returning)

  where

    go
      :: forall returning target row
       . Selects target row
      => TableSchema target
      -> (row -> row)
      -> (row -> Expr Bool)
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
    :: Selects target row
    => { target :: TableSchema target
       , set :: row -> row
       , updateWhere :: row -> Expr Bool
       , returning :: Returning target returning
       }
    -> Update target returning


-- | Exists checks if a query returns at least one row.
--
-- @exists q@ is the same as the SQL expression @EXISTS ( q )@
exists :: Query a -> Query (Expr Bool)
exists = fmap (maybeTable (lit False) (const (lit True))) .
  optional . mapOpaleye Opaleye.restrictExists


-- | Select each row from a table definition.
--
-- This is equivalent to @FROM table@.
each :: Selects schema row => TableSchema schema -> Query row
each = each_forAll


each_forAll
  :: forall schema row
   . Selects schema row
  => TableSchema schema -> Query row
each_forAll schema = liftOpaleye $ Opaleye.selectTableExplicit unpackspec (toOpaleyeTable schema noWriter view)
  where
    noWriter :: Opaleye.Writer () row
    noWriter = Opaleye.Writer $ Opaleye.PackMap \_ _ -> pure ()

    view :: Opaleye.View row
    view = Opaleye.View $ mapTable (mapC (column . columnName)) (tableColumns schema)


-- | Select all rows from another table that match a given predicate. If the
-- predicate is not satisfied, a null 'MaybeTable' is returned.
--
-- @leftJoin t p@ is equivalent to @LEFT JOIN t ON p@.
optional :: Query a -> Query (MaybeTable a)
optional = mapOpaleye $ Opaleye.laterally (Opaleye.QueryArr . go)
  where
    go query (i, left, tag) = (MaybeTable t' a, join, Opaleye.next tag')
      where
        (MaybeTable t a, right, tag') = Opaleye.runSimpleQueryArr (pure <$> query) (i, tag)
        (t', bindings) = Opaleye.run $ Opaleye.runUnpackspec unpackspec (Opaleye.extractAttr "maybe" tag') t
        join = Opaleye.Join Opaleye.LeftJoin (toPrimExpr $ lit True) [] bindings left right


-- | Combine the results of two queries of the same type.
--
-- @union a b@ is the same as the SQL statement @x UNION b@.
union :: Table Expr a => Query a -> Query a -> Query a
union = union_forAll


union_forAll
  :: forall a
   . Table Expr a
  => Query a -> Query a -> Query a
union_forAll l r = liftOpaleye $ Opaleye.unionExplicit binaryspec (toOpaleye l) (toOpaleye r)
  where
    binaryspec :: Opaleye.Binaryspec a a
    binaryspec =
      Opaleye.Binaryspec $ Opaleye.PackMap \f (a, b) ->
        zipTablesWithM (zipCWithM \x y -> fromPrimExpr <$> f (toPrimExpr x, toPrimExpr y)) a b


-- | Select all distinct rows from a query, removing duplicates.
--
-- @distinct q@ is equivalent to the SQL statement @SELECT DISTINCT q@
distinct :: Table Expr a => Query a -> Query a
distinct = distinct_forAll


distinct_forAll :: forall a. Table Expr a => Query a -> Query a
distinct_forAll = mapOpaleye (Opaleye.distinctExplicit distinctspec)
  where
    distinctspec :: Opaleye.Distinctspec a a
    distinctspec =
      Opaleye.Distinctspec $ Opaleye.Aggregator $ Opaleye.PackMap \f ->
        traverseTable (traverseC \x -> fromPrimExpr <$> f (Nothing, toPrimExpr x))


-- | @limit n@ select at most @n@ rows from a query.
--
-- @limit n@ is equivalent to the SQL @LIMIT n@.
limit :: Natural -> Query a -> Query a
limit n = mapOpaleye $ Opaleye.limit (fromIntegral n)


-- | @offset n@ drops the first @n@ rows from a query.
--
-- @offset n@ is equivalent to the SQL @OFFSET n@.
offset :: Natural -> Query a -> Query a
offset n = mapOpaleye $ Opaleye.offset (fromIntegral n)


-- | Drop any rows that don't match a predicate.
--
-- @where_ expr@ is equivalent to the SQL @WHERE expr@.
where_ :: Expr Bool -> Query ()
where_ x =
  liftOpaleye $ Opaleye.QueryArr \((), left, t) ->
    ((), Opaleye.restrict (toPrimExpr x) left, t)


-- | Filter out 'MaybeTable's, returning only the tables that are not-null.
--
-- This operation can be used to "undo" the effect of 'optional', which
-- operationally is like turning a @LEFT JOIN@ back into a full @JOIN@.
catMaybeTable :: MaybeTable a -> Query a
catMaybeTable MaybeTable{ nullTag, table } = do
  where_ $ not_ $ isNull nullTag
  return table


-- | Construct a query that returns the given input list of rows. This is like
-- folding a list of 'return' statements under 'union', but uses the SQL
-- @VALUES@ expression for efficiency.
--
-- Typically @values@ will be used with 'lit':
--
-- @
-- example :: Query Bool
-- example = values [ lit True, lit False ]
-- @
--
-- When selected, 'example' will produce a query that returns two rows - one
-- for @True@ and one for @False@.
values :: forall expr f. (Table Expr expr, Foldable f) => f expr -> Query expr
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


-- | @filter f x@ will be a zero-row query when @f x@ is @False@, and will
-- return @x@ unchanged when @f x@ is @True@. This is similar to
-- 'Control.Monad.guard', but as the predicate is separate from the argument,
-- it is easy to use in a pipeline of 'Query' transformations.
--
-- @
-- data User f = User { ... , userIsDeleted :: Column f Bool }
-- userSchema :: TableSchema (User ColumnSchema)
--
-- notDeletedUsers :: User Expr -> Query (User Expr)
-- notDeletedUsers = filter (not_ . userIsDeleted) =<< each userSchema
-- @
filter :: (a -> Expr Bool) -> a -> Query a
filter f a = do
  where_ $ f a
  return a


-- | Any @Expr@s can be compared for equality as long as the underlying
-- database type supports equality comparisons.
instance DBEq a => EqTable (Expr a) where
  (==.) = eqExprs


{-| The schema for a column in a table. To construct values of this type,
enable the @OverloadedStrings@ language extension and write literal Haskell
strings:

@
\{\-\# LANGUAGE OverloadedStrings -\}
tableSchema :: TableSchema (HaskellPackage ColumnSchema)
tableSchema =
  TableSchema
    { ...
    , tableColumns =
        HaskallPackage
          { packageName = "name" -- Here "name" :: ColumnSchema due to OverloadedStrings
          }
    }
@

If you want to programatically create @ColumnSchema@'s, you can use
'Data.String.fromString':

@
commonPrefix :: String
commonPrefix = "prefix_"

tableSchema :: TableSchema (HaskellPackage ColumnSchema)
tableSchema =
  TableSchema
    { ...
    , tableColumns =
        HaskallPackage
          { packageName = fromString ( prefix ++ "name" )
          }
    }
@

-}
newtype ColumnSchema (a :: Type) =
  ColumnSchema { columnName :: String }


-- | You can construct @ColumnSchema@ values by using @\{\-\# LANGUAGE OverloadedStrings #-\}@ and writing
-- literal strings in your source code.
instance IsString (ColumnSchema a) where
  fromString = ColumnSchema


toOpaleyeTable
  :: TableSchema schema
  -> Opaleye.Writer write view
  -> Opaleye.View view
  -> Opaleye.Table write view
toOpaleyeTable TableSchema{ tableName, tableSchema } writer_ view =
  maybe withoutSchema withSchema tableSchema
  where
    tableFields = Opaleye.TableFields writer_ view

    withoutSchema = Opaleye.Table tableName tableFields
    withSchema s = Opaleye.TableWithSchema s tableName tableFields


data Dict c a where
  Dict :: c a => Dict c a


-- | Convert a query to a 'String' containing the query as a @SELECT@
-- statement.
showQuery :: Table Expr a => Query a -> String
showQuery = fold . selectQuery


data Aggregator a =
  Aggregator
    (Maybe (Opaleye.AggrOp, [Opaleye.OrderExpr], Opaleye.AggrDistinct))
    Opaleye.PrimExpr


data Foldability = 'Semifold | 'Fold


data Aggregate (foldability :: Foldability) a where
  Aggregate :: Expr a -> Aggregator a -> Aggregate foldability a
  Aggregate1 :: Aggregator a -> Aggregate 'Semifold a


-- | Compute the corresponding expression type for an aggregation type.
type family UnAggrType (a :: Type) :: Type where
  UnAggrType (a, b) = (UnAggrType a, UnAggrType b)
  UnAggrType (t (Aggregate _)) = t Expr
  UnAggrType (Aggregate _ a) = Expr a
  UnAggrType (ListTable (Aggregate _) a) = ListTable Expr a
  UnAggrType (NonEmptyTable (Aggregate _) a) = NonEmptyTable Expr a


getAggregator :: Aggregate foldability a -> Aggregator a
getAggregator (Aggregate _ aggregator) = aggregator
getAggregator (Aggregate1 aggregator) = aggregator


groupBy :: Expr a -> Aggregate 'Semifold a
groupBy (Expr a) = Aggregate1 (Aggregator Nothing a)


arrayAgg :: DBType a => Expr a -> Aggregate foldability [a]
arrayAgg (Expr a) = Aggregate (lit []) (Aggregator (Just (Opaleye.AggrArr, [], Opaleye.AggrAll)) a)


arrayAgg1 :: Expr a -> Aggregate 'Semifold (NonEmpty a)
arrayAgg1 (Expr a) = Aggregate1 (Aggregator (Just (Opaleye.AggrArr, [], Opaleye.AggrAll)) a)


class DBType a => DBMax foldability a where
  max :: Expr a -> Aggregate foldability a

  default max :: (foldability ~ 'Semifold) => Expr a -> Aggregate foldability a
  max (Expr a) = Aggregate1 $ Aggregator (Just (Opaleye.AggrMax, [], Opaleye.AggrAll)) a


instance DBMax 'Semifold Int64
instance DBMax 'Semifold Double
instance DBMax 'Semifold Int32
instance DBMax 'Semifold Scientific
instance DBMax 'Semifold Float
instance DBMax 'Semifold Text


instance DBMax 'Semifold a => DBMax foldability (Maybe a) where
  max expr = case getAggregator (max @'Semifold (retype @a expr)) of
    Aggregator a e -> Aggregate (lit Nothing) (Aggregator a e)


aggregate1 ::
  ( Table (Aggregate semifold) aggregates
  , Table Expr exprs
  , Columns aggregates ~ Columns exprs
  , exprs ~ UnAggrType aggregates
  )
  => Query aggregates -> Query exprs
aggregate1 = mapOpaleye $ Opaleye.aggregate $ Opaleye.Aggregator $
  Opaleye.PackMap $ \f ->
    traverseTable $ \(MkC (getAggregator -> Aggregator a e)) -> MkC . Expr <$> f (a, e)


aggregate ::
  ( Table (Aggregate 'Fold) aggregates
  , Table Expr exprs
  , Columns aggregates ~ Columns exprs
  , exprs ~ UnAggrType aggregates
  )
  => Query aggregates -> Query exprs
aggregate aggregates = fmap (maybeTable defaults id) $ optional $ aggregate1 aggregates
  where
    defaults = runIdentity $ traverseTable go $ unquery aggregates
      where
        go (MkC (Aggregate a _)) = pure (MkC a)

        -- HACK: this is very unsafe
        unquery (Query query) = case Opaleye.runSimpleQueryArrStart query () of
          (a, _, _) -> a


newtype ComposeColumn f g a = ComposeColumn (Column f (g a))


traverseComposeColumn :: forall f g t m x. Applicative m => (forall a. C f a -> m (C g a)) -> C (ComposeColumn f t) x -> m (C (ComposeColumn g t) x)
traverseComposeColumn f (MkC (ComposeColumn a)) = f (MkC @f @(t x) a) <&> \case
  MkC b -> MkC (ComposeColumn b)


class c (f a) => ComposeConstraint f c a
instance c (f a) => ComposeConstraint f c a


data HComposeField f t a where
  HComposeField :: HField t a -> HComposeField f t (f a)


newtype HComposeTable g t f = HComposeTable (t (ComposeColumn f g))


instance (HConstrainTable t (ComposeConstraint f DBType), HigherKindedTable t) => HigherKindedTable (HComposeTable f t) where
  type HField (HComposeTable f t) = HComposeField f t
  type HConstrainTable (HComposeTable f t) c = HConstrainTable t (ComposeConstraint f c)

  hfield (HComposeTable columns) (HComposeField field) = case hfield columns field of
    MkC (ComposeColumn a) -> MkC a

  htabulate f = HComposeTable (htabulate (\field -> MkC (ComposeColumn (toColumn (f (HComposeField field))))))

  htraverse f (HComposeTable t) = fmap HComposeTable $ htraverse (traverseComposeColumn f) t

  hdicts :: forall c. HConstrainTable t (ComposeConstraint f c) => HComposeTable f t (Dict c)
  hdicts = HComposeTable $ runIdentity $ htraverse (\(MkC Dict) -> pure (MkC (ComposeColumn Dict))) (hdicts @t @(ComposeConstraint f c))


newtype ListTable f a = ListTable (Columns a (ComposeColumn f []))


instance (HConstrainTable (Columns a) (ComposeConstraint [] DBType), Table Expr a) => Table f (ListTable f a) where
  type Columns (ListTable f a) = HComposeTable [] (Columns a)

  toColumns (ListTable a) = HComposeTable a
  fromColumns (HComposeTable a) = ListTable a


instance
  ( expr ~ Expr
  , ExprType [b] ~ ListTable expr a
  , Serializable a b
  , HConstrainTable (Columns a) (ComposeConstraint [] DBType)
  ) => Serializable (ListTable expr a) [b]
 where

  rowParser inject = fmap getZipList . getCompose <$> rowParser @a \fieldParser x y ->
    Compose . fmap pgArrayToZipList <$> inject (pgArrayFieldParser fieldParser) x y
    where
      pgArrayToZipList :: forall x. PGArray x -> ZipList x
      pgArrayToZipList (PGArray a) = ZipList a


  lit (map lit -> xs) = ListTable $ htabulate $ \field ->
    case hfield dbtypes field of
      MkC Dict -> MkC $ ComposeColumn $ listOf $
        map (\x -> toColumn (hfield (toColumns x) field)) xs
    where
      dbtypes :: Columns a (Dict DBType)
      dbtypes = hdicts

      listOf :: forall x. DBType x => [Expr x] -> Expr [x]
      listOf as = fromPrimExpr $
        Opaleye.CastExpr array $
        Opaleye.ArrayExpr (map toPrimExpr as)
        where
          array = typeName (typeInformation @[x])


many :: forall exprs foldability. Table Expr exprs => exprs -> ListTable (Aggregate foldability) exprs
many exprs = ListTable $ htabulate $ \field -> case hfield constraints field of
  MkC Dict -> MkC $ ComposeColumn $ case hfield (toColumns exprs) field of
    MkC a -> arrayAgg a
  where
    constraints :: Columns exprs (Dict DBType)
    constraints = hdicts


newtype NonEmptyTable f a = NonEmptyTable (Columns a (ComposeColumn f NonEmpty))


instance (HConstrainTable (Columns a) (ComposeConstraint NonEmpty DBType), Table Expr a) => Table f (NonEmptyTable f a) where
  type Columns (NonEmptyTable f a) = HComposeTable NonEmpty (Columns a)

  toColumns (NonEmptyTable a) = HComposeTable a
  fromColumns (HComposeTable a) = NonEmptyTable a


instance
  ( expr ~ Expr
  , ExprType (NonEmpty b) ~ NonEmptyTable expr a
  , Serializable a b
  , HConstrainTable (Columns a) (ComposeConstraint NonEmpty DBType)
  ) => Serializable (NonEmptyTable expr a) (NonEmpty b)
 where

  rowParser inject = fmap (NonEmpty.fromList . getZipList) . getCompose <$> rowParser @a \fieldParser x y ->
      Compose . fmap pgArrayToZipList <$> inject (pgNonEmptyFieldParser fieldParser) x y
    where
      pgArrayToZipList :: forall x. PGArray x -> ZipList x
      pgArrayToZipList (PGArray a) = ZipList a

      pgNonEmptyFieldParser parser x y = do
        list <- pgArrayFieldParser parser x y
        case list of
          PGArray [] -> returnError Incompatible x "Serializable.NonEmptyTable.rowParser: empty list"
          _ -> pure list


  lit (fmap lit -> xs) = NonEmptyTable $ htabulate $ \field ->
    case hfield dbtypes field of
      MkC Dict -> MkC $ ComposeColumn $ nonEmptyOf $
        fmap (\x -> toColumn (hfield (toColumns x) field)) xs
    where
      dbtypes :: Columns a (Dict DBType)
      dbtypes = hdicts

      nonEmptyOf :: forall x. DBType x => NonEmpty (Expr x) -> Expr (NonEmpty x)
      nonEmptyOf as = fromPrimExpr $
        Opaleye.CastExpr array $
        Opaleye.ArrayExpr (map toPrimExpr (toList as))
        where
          array = typeName (typeInformation @(NonEmpty x))


some :: Table Expr exprs => exprs -> NonEmptyTable (Aggregate 'Semifold) exprs
some exprs = NonEmptyTable $ runIdentity $ traverseTable go exprs
  where
    go (MkC a) = pure (MkC (ComposeColumn (arrayAgg1 a)))


{-| An ordering expression for @a@. Primitive orderings are defined with 'asc'
and 'desc', and you can combine @Order@ via its various instances.

A common pattern is to use '<>' to combine multiple orderings in sequence, and '>$<' (from 'Contravariant') to select individual columns. For example, to sort a @Query@ on two columns, we could do:

@
orderExample :: Query (Expr Int, Expr Bool) -> Query (Expr Int, Expr Bool)
orderExample = orderBy (fst >$< asc <> snd >$< desc)
@
-}
newtype Order a = Order (Opaleye.Order a)
  deriving newtype (Contravariant, Divisible, Decidable, Semigroup, Monoid)


{-| Sort a table in ascending order.
-}
asc :: Table Expr a => Order a
asc = Order $ Opaleye.Order (getConst . htraverse f . toColumns)
  where
    f :: forall x. C Expr x -> Const [(Opaleye.OrderOp, Opaleye.PrimExpr)] (C Expr x)
    f (MkC (Expr primExpr)) = Const [(orderOp, primExpr)]

    orderOp :: Opaleye.OrderOp
    orderOp = Opaleye.OrderOp 
      { orderDirection = Opaleye.OpAsc
        , orderNulls = Opaleye.NullsLast 
      }


{-| Sort a table in descending order.
-}
desc :: Table Expr a => Order a
desc = Order $ Opaleye.Order (getConst . htraverse f . toColumns)
  where
    f :: forall x. C Expr x -> Const [(Opaleye.OrderOp, Opaleye.PrimExpr)] (C Expr x)
    f (MkC (Expr primExpr)) = Const [(orderOp, primExpr)]

    orderOp :: Opaleye.OrderOp
    orderOp = Opaleye.OrderOp 
      { orderDirection = Opaleye.OpDesc
        , orderNulls = Opaleye.NullsFirst 
      }


{-| Transform an ordering so that @null@ values appear first.
-}
nullsFirst :: Order (Expr (Maybe a)) -> Order (Expr (Maybe a))
nullsFirst (Order (Opaleye.Order f)) = Order $ Opaleye.Order $ fmap (first g) . f
  where
    g :: Opaleye.OrderOp -> Opaleye.OrderOp
    g orderOp = orderOp { Opaleye.orderNulls = Opaleye.NullsFirst }


{-| Transform an ordering so that @null@ values appear first.
-}
nullsLast :: Order (Expr (Maybe a)) -> Order (Expr (Maybe a))
nullsLast (Order (Opaleye.Order f)) = Order $ Opaleye.Order $ fmap (first g) . f
  where
    g :: Opaleye.OrderOp -> Opaleye.OrderOp
    g orderOp = orderOp { Opaleye.orderNulls = Opaleye.NullsLast }


{-| Order the rows returned by a 'Query' according to a particular 'Order'.
-}
orderBy :: Order a -> Query a -> Query a
orderBy (Order o) = liftOpaleye . Opaleye.laterally (Opaleye.orderBy o) . toOpaleye


-- | We say that two 'Table's are congruent if they have the same set of
-- columns. This is primarily useful for operations like @SELECT FROM@, where
-- we have a @Table@ of @ColumnSchema@s, and need to select them to a
-- corresponding @Table@ of @Expr@s.
class (Columns a ~ Columns b) => Congruent a b
instance (Columns a ~ Columns b) => Congruent a b


-- | We say that @Table a@ "selects" @Table b@ if @a@ and @b@ are 'Congruent',
-- @a@ contains 'ColumnSchema's and @b@ contains 'Expr's.
class (Congruent schema exprs, Table Expr exprs, Table ColumnSchema schema) => Selects schema exprs
instance (Congruent schema exprs, Table Expr exprs, Table ColumnSchema schema) => Selects schema exprs
