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
import Rel8.Expr ( Expr )
import Rel8.Kind.Labels ( Labels )
import Rel8.Kind.Necessity ( Necessity( Required, Optional ) )
import Rel8.Schema.Field ( Field )
import Rel8.Schema.Insert ( Insert )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Name ( Name )
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
type Column :: K.Context -> Type -> Type
type Column context a =
  Field context (GetLabel a)
    (GetNecessity (UnwrapLabel a))
    (UnwrapDefault (UnwrapLabel a))


type HEither :: K.Context -> Type -> Type -> Type
type family HEither context where
  HEither Structure = Shape2 'Either
  HEither Expr = EitherTable
  HEither Identity = Either
  HEither Insert = EitherTable
  HEither Name = EitherTable
  HEither _ = Either


type HList :: K.Context -> Type -> Type
type family HList context where
  HList Structure = Shape1 'List
  HList Expr = ListTable
  HList Identity = []
  HList Insert = ListTable
  HList Name = ListTable
  HList _ = []


type HMaybe :: K.Context -> Type -> Type
type family HMaybe context where
  HMaybe Structure = Shape1 'Maybe
  HMaybe Expr = MaybeTable
  HMaybe Identity = Maybe
  HMaybe Insert = MaybeTable
  HMaybe Name = MaybeTable
  HMaybe _ = Maybe


type HNonEmpty :: K.Context -> Type -> Type
type family HNonEmpty context where
  HNonEmpty Structure = Shape1 'NonEmpty
  HNonEmpty Expr = NonEmptyTable
  HNonEmpty Identity = NonEmpty
  HNonEmpty Insert = NonEmptyTable
  HNonEmpty Name = NonEmptyTable
  HNonEmpty _ = NonEmpty


type HThese :: K.Context -> Type -> Type -> Type
type family HThese context where
  HThese Structure = Shape2 'These
  HThese Expr = TheseTable
  HThese Identity = These
  HThese Insert = TheseTable
  HThese Name = TheseTable
  HThese _ = These
