{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilyDependencies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

{-| Every relational library also tries to build some type of record system,
and Rel8 is no exception! We've tried hard to make sure you don't have to
understand this module to enjoy using Rel8 - in fact, you can probably close
your browser tab now if you want to! However, this is the API that's used
behind the scenes, and is safely exported if you want to use it in your own
work, or if you want to understand further how Rel8 works.

-}
module Rel8.Table
  ( -- * Tables of kind @*@
    Table(..)
  , mapTable
  , mapTableC
  , traverseTable
  , traverseTableC
  , traverseTableWithIndexC
  , zipTablesWithM
  , zipTablesWithMC

    -- ** Sub-tables
  , Unconstrained

    -- ** Relationships Between Tables
  , Recontextualise(..)

    -- * Columns
  , Column
  , C( MkC )
  , mapC
  , traverseC
  , traverseCC
  , zipCWithM
  , zipCWithMC
  ) where

import Data.Functor.Compose
import Data.Functor.Identity
import Data.Kind
import Data.Monoid
import Data.Proxy
import GHC.Exts ( Constraint )
import Rel8.Column
import Rel8.DBType
import Rel8.Unconstrained


{-| Types that represent SQL tables.

You generally should not need to derive instances of this class manually, as
writing higher-kinded data types is usually more convenient. See also:
'HigherKindedTable'.

== __Theory__

Theoretically, the @Table@ type class really describes "higher-kinded
representable functors". A representable functor in Haskell is usually described
by the type class:

@
class Functor f => Representable f where
  type Rep f :: *
  index :: f a -> Rep f -> a
  tabulate :: ( Rep f -> a ) -> f a
@

That is, a representable functor is some functor where you can talk about the
"locations" of the @a@s that it holds, allowing you to pull individual @a@s out,
and to construct @f a@'s by repeatedly calling a function for every location.
While this seems like a lot of work, being @Representable@ has a lot of nice
properties - it means the functor holds a finite number of elements (known
statically), and it means we can recover a lot of other operations - the are
applicative, monadic, zippable, and more.

To see the connection to @Rel8@'s table type, let's build a small representable
functor for Haskell packages:

@
data HaskellPackage a = HaskellPackage { packageId, packageName :: a }
  deriving Functor

data HaskellPackageField = HaskellPackageId | HaskellPackageName

instance Representable HaskellPackage where
  type Rep HaskellPackage = HaskellPackageField
  index HaskellPackage{ packageId, packageName } = \\case
    HaskellPackageId -> packageId
    HaskellPackageName -> packageName
@

Hopefully you can see that for records, @Representable@ really speaks about
records where all fields of the record can be accessed by some kind of first
class name. However, for our purposes @Representable@ isn't enough - it means
/all/ fields in the record have to share the same type, but that's not what
we want!

@Table@ is the solution, which comes from (mostly) mechanically lifting
@Representable@ up to another kind. Now, our @Rep@ (@Field@ in @Table@) isn't
of type @*@, but it's now of type @* -> *@. This lets the @Field@ carry type
information:

@
data HaskellPackage = HaskellPackage { packageId :: Int, packageName :: String }
  deriving Functor

data HaskellPackageField :: * -> * where
  HaskellPackageId   :: HaskellPackageField Int
  HaskellPackageName :: HaskellPackageName String

instance Table HaskellPackage where
  type Field HaskellPackage = HaskellPackageField
  field HaskellPackage{ packageId, packageName } = \\case
    HaskellPackageId -> packageId
    HaskellPackageName -> packageName
@

-}
class (ConstrainTable t DBType, ConstrainTable t Unconstrained) => Table ( t :: Type ) where
  -- | The @Field@ type is a type where each value corresponds to a distinct
  -- field in the table. It describes not just the field itself, but also the
  -- type of values stored there.
  type Field t = ( field :: Type -> Type ) | field -> t

  -- | All fields in a table are intepreted under a common functor @f@.
  -- This associated type family lets us extract that functor.
  type Context t :: Type -> Type

  -- | Ensure a constraint holds over all field types in the table.
  type ConstrainTable t ( c :: Type -> Constraint ) :: Constraint

  -- | Give the tag of field in the table, look at the contents of that field.
  field :: t -> Field t x -> C ( Context t ) x

  -- | Given a function that knows how to construct fields in the 'Context' of
  -- the table @t@, build a @t@ by calling that function for every field.
  -- The function can also request constraints to hold on all @x@s in the
  -- structure by using 'ConstrainTable'.
  --
  -- MCP stands for "monadic", "constrained" and "by proxy". The various other
  -- tabulate functions will probably be more convenient, but this the
  -- fundamental operator.
  tabulateMCP
    :: forall c f proxy
     . ( ConstrainTable t c, Applicative f )
    => proxy c
    -> ( forall x. c x => Field t x -> f ( C ( Context t ) x ) )
    -> f t


class ( Table t, Table ( MapTable f t ) ) => Recontextualise ( t :: Type ) ( f :: ( Type -> Type ) -> Type -> Type ) where
  type MapTable f t :: Type

  fieldMapping :: Field ( MapTable f t ) x -> Field t x

  reverseFieldMapping :: Field t x -> Field ( MapTable f t ) x


-- | Effectfully map a table from one context to another.
traverseTableWithIndexC
  :: forall c f t t' m
   . ( Applicative m
     , ConstrainTable t' c
     , MapTable f t ~ t'
     , Recontextualise t f
     )
  => ( forall x. c x => Field t x -> C ( Context t ) x -> m ( C ( Context t' ) x ) )
  -> t
  -> m t'
traverseTableWithIndexC f t =
  tabulateMCP ( Proxy @c ) \index ->
    f ( fieldMapping @_ @f index ) ( field t ( fieldMapping @_ @f index ) )


data TupleField a b x where
  Element1 :: Field a x -> TupleField a b x
  Element2 :: Field b x -> TupleField a b x


-- | The product of two tables is also a table, provided that they share the
-- same 'Context'.
instance ( Context a ~ Context b, Table a, Table b ) => Table ( a, b ) where
  type Context ( a, b ) =
    Context a

  type ConstrainTable ( a, b ) c =
    ( ConstrainTable a c, ConstrainTable b c )

  type Field ( a, b ) = TupleField a b

  field ( a, b ) = \case
    Element1 f -> field a f
    Element2 f -> field b f

  tabulateMCP proxy f =
    (,) <$> tabulateMCP proxy ( f . Element1 )
        <*> tabulateMCP proxy ( f . Element2 )


instance ( Context a ~ Context b, Context ( MapTable f a ) ~ Context ( MapTable f b ), Recontextualise a f, Recontextualise b f ) => Recontextualise ( a, b ) f where
  type MapTable f ( a, b ) =
    ( MapTable f a, MapTable f b )

  fieldMapping = \case
    Element1 i -> Element1 ( fieldMapping @a @f i )
    Element2 i -> Element2 ( fieldMapping @b @f i )

  reverseFieldMapping = \case
    Element1 i -> Element1 ( reverseFieldMapping @a @f i )
    Element2 i -> Element2 ( reverseFieldMapping @b @f i )


data SumField a x where
  SumField :: Field a x -> SumField a x


instance Table a => Table ( Sum a ) where
  type ConstrainTable ( Sum a ) c =
    ConstrainTable a c

  type Context ( Sum a ) =
    Context a

  type Field ( Sum a ) = SumField a

  field ( Sum a ) ( SumField i ) =
    field a i

  tabulateMCP proxy f =
    Sum <$> tabulateMCP proxy ( f . SumField )


instance Recontextualise a f => Recontextualise ( Sum a ) f where
  type MapTable f ( Sum a ) =
    Sum ( MapTable f a )

  fieldMapping ( SumField i ) =
    SumField ( fieldMapping @_ @f i )

  reverseFieldMapping ( SumField i ) =
    SumField ( reverseFieldMapping @_ @f i )


-- | Map a 'Table' from one type to another. The table types must be compatible,
-- see 'Compatible' for what that means.
mapTable
  :: forall f t' t
   . ( MapTable f t ~ t'
     , Recontextualise t f
     )
  => ( forall x. C ( Context t ) x -> C ( Context t' ) x ) -> t -> t'
mapTable f =
  runIdentity . traverseTable @f ( Identity . f )


instance Table a => Table ( Identity a ) where
  type Context ( Identity a ) =
    Context a

  type ConstrainTable ( Identity a ) c =
    ConstrainTable a c

  type Field ( Identity a ) =
    Compose Identity ( Field a )

  field ( Identity a ) ( Compose ( Identity x ) ) =
    field a x

  tabulateMCP proxy f =
    Identity <$> tabulateMCP proxy ( f . Compose . Identity )


instance Table a => Recontextualise ( Identity a ) Id where
  type MapTable Id ( Identity a ) =
    Identity a

  fieldMapping ( Compose ( Identity i ) ) =
    Compose ( Identity i )

  reverseFieldMapping ( Compose ( Identity i ) ) =
    Compose ( Identity i )


-- | Map a 'Table' from one type to another, where all columns in the table are
-- subject to a constraint. The table types must be compatible, see 'Compatible'
-- for what that means.
mapTableC
  :: forall c f t' t
   . ( ConstrainTable t' c, MapTable f t ~ t', Recontextualise t f )
  => ( forall x. c x => C ( Context t ) x -> C ( Context t' ) x ) -> t -> t'
mapTableC f =
  runIdentity . traverseTableC @f @c ( Identity . f )


-- | Effectfully traverse all fields in a 'Table', potentially producing another
-- @Table@.
traverseTable
  :: forall f t' t m
   . ( Applicative m, MapTable f t ~ t', Recontextualise t f )
  => ( forall x. C ( Context t ) x -> m ( C ( Context t' ) x ) )
  -> t
  -> m t'
traverseTable f =
  traverseTableWithIndexC @Unconstrained @f ( const f )


-- | Effectfully traverse all fields in a 'Table', provided that all fields
-- satisfy a given constraint. For example, if all fields in a table have an
-- instance for 'Read', we can apply 'readMaybe' to all fields in the table,
-- failing if any read fails:
--
-- >>> traverseTableC @Read ( traverseC readMaybe ) MyTable{ fieldA = "True" }
-- Just MyTable{ fieldA = True }
traverseTableC
  :: forall f c m t t'
   . ( Applicative m, MapTable f t ~ t', ConstrainTable t' c, Recontextualise t f )
  => ( forall x. c x => C ( Context t ) x -> m ( C ( Context t' ) x ) )
  -> t
  -> m t'
traverseTableC f =
  traverseTableWithIndexC @c @f ( const f )


zipTablesWithM
  :: forall t m
   . ( Applicative m
     , Table t
     )
  => ( forall x. C ( Context t ) x -> C ( Context t ) x -> m ( C ( Context t ) x ) )
  -> t -> t -> m t
zipTablesWithM f t t' =
  tabulateMCP ( Proxy @Unconstrained ) \index ->
    f ( field t index ) ( field t' index )


zipTablesWithMC
  :: forall c t m
   . ( Applicative m, ConstrainTable t c, Table t )
  => ( forall x. c x => C ( Context t ) x -> C ( Context t ) x -> m ( C ( Context t ) x ) )
  -> t -> t -> m t
zipTablesWithMC f t t' =
  tabulateMCP @t ( Proxy @c) \index ->
    f ( field t index ) ( field t' index )
