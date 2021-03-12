{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language TypeFamilyDependencies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Schema.Column
  ( Column
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
import Rel8.Expr ( Expr )
import Rel8.Expr.Aggregate ( Aggregate )
import Rel8.Kind.Necessity ( Necessity( Required, Optional ) )
import Rel8.Kind.Nullability ( Nullability ( Nullable, NonNullable ) )
import Rel8.Schema.Context
  ( Aggregation, DB, Insert, Result
  , IsSpecialContext
  )
import Rel8.Schema.Spec ( Context, Spec( Spec ) )
import Rel8.Schema.Structure
  ( Structure
  , Shape( Column, Either, List, Maybe, NonEmpty, These )
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


type IColumn :: Bool -> Context -> Necessity -> Nullability -> Type -> Type
type family IColumn isSpecialContext context necessity nullability a where
  IColumn 'False context     necessity  nullability  a = context ('Spec necessity nullability a)
  IColumn 'True  Result      _necessity 'NonNullable a = a
  IColumn 'True  Result      _necessity 'Nullable    a = Maybe a
  IColumn 'True  DB          _necessity nullability  a = Expr nullability a
  IColumn 'True  Insert      'Required  nullability  a = Expr nullability a
  IColumn 'True  Insert      'Optional  nullability  a = Maybe (Expr nullability a)
  IColumn 'True  Aggregation _necessity nullability  a = Aggregate nullability a
  IColumn 'True  Structure   necessity  nullability  a = Shape1 'Column ('Spec necessity nullability a)


type IHEither :: Bool -> Context -> Type -> Type -> Type
type family IHEither isSpecialContext context = either where
  IHEither 'False _ = EitherTable
  IHEither 'True Result = Either
  IHEither 'True Structure = Shape2 'Either
  IHEither 'True _ = EitherTable


type IHList :: Bool -> Context -> Type -> Type
type family IHList isSpecialContext context = list where
  IHList 'False _ = ListTable
  IHList 'True Result = []
  IHList 'True Structure = Shape1 'List
  IHList 'True _ = ListTable


type IHMaybe :: Bool -> Context -> Type -> Type
type family IHMaybe isSpecialContext context = maybe where
  IHMaybe 'False _ = MaybeTable
  IHMaybe 'True Result = Maybe
  IHMaybe 'True Structure = Shape1 'Maybe
  IHMaybe 'True _ = MaybeTable


type IHNonEmpty :: Bool -> Context -> Type -> Type
type family IHNonEmpty isSpecialContext context = nonEmpty where
  IHNonEmpty 'False _ = NonEmptyTable
  IHNonEmpty 'True Result = NonEmpty
  IHNonEmpty 'True Structure = Shape1 'NonEmpty
  IHNonEmpty 'True _ = NonEmptyTable


type IHThese :: Bool -> Context -> Type -> Type -> Type
type family IHThese isSpecialContext context = these where
  IHThese 'False _ = TheseTable
  IHThese 'True Result = These
  IHThese 'True Structure = Shape2 'These
  IHThese 'True _ = TheseTable


type Column :: Context -> Necessity -> Nullability -> Type -> Type
type Column context necessity nullability a =
  IColumn (IsSpecialContext context) context necessity nullability a


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
