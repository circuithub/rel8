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
import Rel8.Kind.Labels ( Labels )
import Rel8.Kind.Necessity ( Necessity( Required, Optional ) )
import Rel8.Schema.Context ( IsSpecialContext )
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


type Column :: K.Context -> Type -> Type
type Column context a =
  Field context (GetLabel a)
    (GetNecessity (UnwrapLabel a))
    (Unnullify (UnwrapDefault (UnwrapLabel a)))
    (UnwrapDefault (UnwrapLabel a))


type IHEither :: Bool -> K.Context -> Type -> Type -> Type
type family IHEither isSpecialContext context where
  IHEither 'False _ = EitherTable
  IHEither 'True Identity = Either
  IHEither 'True Structure = Shape2 'Either
  IHEither 'True _ = EitherTable


type IHList :: Bool -> K.Context -> Type -> Type
type family IHList isSpecialContext context where
  IHList 'False _ = ListTable
  IHList 'True Identity = []
  IHList 'True Structure = Shape1 'List
  IHList 'True _ = ListTable


type IHMaybe :: Bool -> K.Context -> Type -> Type
type family IHMaybe isSpecialContext context where
  IHMaybe 'False _ = MaybeTable
  IHMaybe 'True Identity = Maybe
  IHMaybe 'True Structure = Shape1 'Maybe
  IHMaybe 'True _ = MaybeTable


type IHNonEmpty :: Bool -> K.Context -> Type -> Type
type family IHNonEmpty isSpecialContext context where
  IHNonEmpty 'False _ = NonEmptyTable
  IHNonEmpty 'True Identity = NonEmpty
  IHNonEmpty 'True Structure = Shape1 'NonEmpty
  IHNonEmpty 'True _ = NonEmptyTable


type IHThese :: Bool -> K.Context -> Type -> Type -> Type
type family IHThese isSpecialContext context where
  IHThese 'False _ = TheseTable
  IHThese 'True Identity = These
  IHThese 'True Structure = Shape2 'These
  IHThese 'True _ = TheseTable


type HEither :: K.Context -> Type -> Type -> Type
type HEither context = IHEither (IsSpecialContext context) context


type HList :: K.Context -> Type -> Type
type HList context = IHList (IsSpecialContext context) context


type HMaybe :: K.Context -> Type -> Type
type HMaybe context = IHMaybe (IsSpecialContext context) context


type HNonEmpty :: K.Context -> Type -> Type
type HNonEmpty context = IHNonEmpty (IsSpecialContext context) context


type HThese :: K.Context -> Type -> Type -> Type
type HThese context = IHThese (IsSpecialContext context) context
