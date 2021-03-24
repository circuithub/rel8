{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.Column
  ( Column, Default, Label
  , HEither
  , HList
  , HMaybe
  , HNonEmpty
  , HThese
  )
where

-- base
import Data.Functor.Identity ( Identity )
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty )
import GHC.TypeLits ( Symbol )
import Prelude

-- rel8
import Rel8.Aggregate ( Aggregate )
import Rel8.Expr ( Expr )
import Rel8.Kind.Labels ( Labels )
import Rel8.Kind.Necessity ( Necessity( Required, Optional ) )
import Rel8.Schema.Context ( Name, Insertion )
import Rel8.Schema.Field ( Field )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Nullability ( Unnullify )
import Rel8.Schema.Structure
  ( Structure
  , Shape( Either, List, Maybe, NonEmpty, These )
  , Shape1
  , Shape2
  )
import Rel8.Table.Either ( EitherTable )
import Rel8.Table.List ( ListTable )
import Rel8.Table.Maybe ( MaybeTable )
import Rel8.Table.NonEmpty ( NonEmptyTable )
import Rel8.Table.These ( TheseTable )

-- these
import Data.These ( These )


type Label :: Symbol -> Type -> Type
data Label label a


type Default :: Type -> Type
data Default a


type GetLabel :: Type -> Labels
type family GetLabel a where
  GetLabel (Label label _) = '[label]
  GetLabel _ = '[]


type UnwrapLabel :: Type -> Type
type family UnwrapLabel a where
  UnwrapLabel (Label _ a) = a
  UnwrapLabel a = a


type GetNecessity :: Type -> Necessity
type family GetNecessity a where
  GetNecessity (Default _) = 'Optional
  GetNecessity _ = 'Required


type UnwrapDefault :: Type -> Type
type family UnwrapDefault a where
  UnwrapDefault (Default a) = a
  UnwrapDefault a = a


-- | The @Column@ type family should be used to indicate which fields of your
-- data types are single columns in queries. This type family has special
-- support when a query is executed, allowing you to use a single data type for
-- both query data and rows decoded to Haskell.
-- 
-- To understand why this type family is special, let's consider a simple
-- higher-kinded data type of Haskell packages:
-- 
-- >>> :{
-- data Package f = Package
--   { packageName   :: Column f Text
--   , packageAuthor :: Column f Text
--   }
-- :}
-- 
-- In queries, @f@ will be some type of 'Expr', and @Column Expr a@ reduces to
-- just @Expr a@:
-- 
-- >>> :t packageName (undefined :: Package Expr)
-- packageName (undefined :: Package Expr) :: Expr Text
-- 
-- When we 'select' queries of this type, @f@ will be instantiated as
-- @Identity@, at which point all wrapping entire disappears:
-- 
-- >>> :t packageName (undefined :: Package Identity)
-- packageName (undefined :: Package Identity) :: Text
-- 
-- In @rel8@ we try hard to always know what @f@ is, which means holes should
-- mention precise types, rather than the @Column@ type family. You should only
-- need to be aware of the type family when defining your table types.
type Column :: K.Context -> Type -> Type
type Column context a =
  Field context (GetLabel a)
    (GetNecessity (UnwrapLabel a))
    (Unnullify (UnwrapDefault (UnwrapLabel a)))
    (UnwrapDefault (UnwrapLabel a))


type HEither :: K.Context -> Type -> Type -> Type
type family HEither context where
  HEither Structure = Shape2 'Either
  HEither Aggregate = EitherTable
  HEither Expr = EitherTable
  HEither Identity = Either
  HEither Insertion = EitherTable
  HEither Name = EitherTable
  HEither _ = Either


type HList :: K.Context -> Type -> Type
type family HList context where
  HList Structure = Shape1 'List
  HList Aggregate = ListTable
  HList Expr = ListTable
  HList Identity = []
  HList Insertion = ListTable
  HList Name = ListTable
  HList _ = []


type HMaybe :: K.Context -> Type -> Type
type family HMaybe context where
  HMaybe Structure = Shape1 'Maybe
  HMaybe Aggregate = MaybeTable
  HMaybe Expr = MaybeTable
  HMaybe Identity = Maybe
  HMaybe Insertion = MaybeTable
  HMaybe Name = MaybeTable
  HMaybe _ = Maybe


type HNonEmpty :: K.Context -> Type -> Type
type family HNonEmpty context where
  HNonEmpty Structure = Shape1 'NonEmpty
  HNonEmpty Aggregate = NonEmptyTable
  HNonEmpty Expr = NonEmptyTable
  HNonEmpty Identity = NonEmpty
  HNonEmpty Insertion = NonEmptyTable
  HNonEmpty Name = NonEmptyTable
  HNonEmpty _ = NonEmpty


type HThese :: K.Context -> Type -> Type -> Type
type family HThese context where
  HThese Structure = Shape2 'These
  HThese Aggregate = TheseTable
  HThese Expr = TheseTable
  HThese Identity = These
  HThese Insertion = TheseTable
  HThese Name = TheseTable
  HThese _ = These
