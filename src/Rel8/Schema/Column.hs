{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.Column
  ( Column, Default
  , HEither
  , HList
  , HMaybe
  , HNonEmpty
  , HThese
  )
where

-- base
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- rel8
import Rel8.Kind.Blueprint ( FromType )
import Rel8.Kind.Necessity ( Necessity( Required, Optional ) )
import Rel8.Schema.Context ( Result, IsSpecialContext )
import Rel8.Schema.Field ( Field )
import Rel8.Schema.Spec ( Context )
import Rel8.Schema.Structure
  ( Structure
  , Shape( Either, List, Maybe, NonEmpty, These )
  , Shape1
  , Shape2
  )
import Rel8.Schema.Value ( GetNullability, GetValue )
import Rel8.Table.Either ( EitherTable )
import Rel8.Table.List ( ListTable )
import Rel8.Table.Maybe ( MaybeTable )
import Rel8.Table.NonEmpty ( NonEmptyTable )
import Rel8.Table.These ( TheseTable )

-- these
import Data.These ( These )


type Default :: Type -> Type
data Default a


type GetNecessity :: Type -> Necessity
type family GetNecessity a where
  GetNecessity (Default _) = 'Optional
  GetNecessity _ = 'Required


type UnwrapDefault :: Type -> Type
type family UnwrapDefault a where
  UnwrapDefault (Default a) = a
  UnwrapDefault a = a


type Column :: Context -> Type -> Type
type Column context a =
  Field context
    (GetNecessity a)
    (GetNullability (UnwrapDefault a))
    (FromType (GetValue (GetNullability (UnwrapDefault a)) (UnwrapDefault a)))


type IHEither :: Bool -> Context -> Type -> Type -> Type
type family IHEither isSpecialContext context where
  IHEither 'False _ = EitherTable
  IHEither 'True Result = Either
  IHEither 'True Structure = Shape2 'Either
  IHEither 'True _ = EitherTable


type IHList :: Bool -> Context -> Type -> Type
type family IHList isSpecialContext context where
  IHList 'False _ = ListTable
  IHList 'True Result = []
  IHList 'True Structure = Shape1 'List
  IHList 'True _ = ListTable


type IHMaybe :: Bool -> Context -> Type -> Type
type family IHMaybe isSpecialContext context where
  IHMaybe 'False _ = MaybeTable
  IHMaybe 'True Result = Maybe
  IHMaybe 'True Structure = Shape1 'Maybe
  IHMaybe 'True _ = MaybeTable


type IHNonEmpty :: Bool -> Context -> Type -> Type
type family IHNonEmpty isSpecialContext context where
  IHNonEmpty 'False _ = NonEmptyTable
  IHNonEmpty 'True Result = NonEmpty
  IHNonEmpty 'True Structure = Shape1 'NonEmpty
  IHNonEmpty 'True _ = NonEmptyTable


type IHThese :: Bool -> Context -> Type -> Type -> Type
type family IHThese isSpecialContext context where
  IHThese 'False _ = TheseTable
  IHThese 'True Result = These
  IHThese 'True Structure = Shape2 'These
  IHThese 'True _ = TheseTable


type HEither :: Context -> Type -> Type -> Type
type HEither context = IHEither (IsSpecialContext context) context


type HList :: Context -> Type -> Type
type HList context = IHList (IsSpecialContext context) context


type HMaybe :: Context -> Type -> Type
type HMaybe context = IHMaybe (IsSpecialContext context) context


type HNonEmpty :: Context -> Type -> Type
type HNonEmpty context = IHNonEmpty (IsSpecialContext context) context


type HThese :: Context -> Type -> Type -> Type
type HThese context = IHThese (IsSpecialContext context) context
