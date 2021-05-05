{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

{-# options_ghc -fno-warn-orphans #-}

module Rel8.Table.Rel8able
  (
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import qualified Rel8.Kind.Algebra as K
import Rel8.Kind.Context
  ( SContext( SReify )
  , Reifiable, contextSing
  , sLabelable, sReifiable
  )
import Rel8.Generic.Rel8able
  ( Rel8able, Algebra
  , GColumns, gfromColumns, gtoColumns
  , greify, gunreify
  )
import Rel8.Schema.Context ( Col )
import Rel8.Schema.Context.Label ( Labelable )
import Rel8.Schema.Dict ( Dict( Dict ) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.HTable ( HConstrainTable, hdicts )
import Rel8.Schema.Reify ( hreify, hunreify, UnwrapReify )
import Rel8.Schema.Result ( Result )
import Rel8.Table
  ( Table, Columns, Context, Congruent, fromColumns, toColumns
  , Unreify, reify, unreify
  )
import Rel8.Schema.Spec.ConstrainDBType ( ConstrainDBType )
import Rel8.Table.ADT ( ADT( ADT ), ADTable, fromADT, toADT )
import Rel8.Table.Eq ( EqTable, eqTable )
import Rel8.Table.HKD ( HKD )
import Rel8.Table.Ord ( OrdTable, ordTable )
import Rel8.Table.Recontextualize ( Recontextualize )
import Rel8.Table.Serialize ( FromExprs, ToExprs, fromResult, toResult )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Ord ( DBOrd )


instance (Rel8able t, Labelable context, Reifiable context) =>
  Table context (t context)
 where
  type Columns (t context) = GColumns t
  type Context (t context) = context
  type Unreify (t context) = t (UnwrapReify context)

  fromColumns = gunreify . gfromColumns . hreify
  toColumns = hunreify . gtoColumns . greify

  reify Refl = case contextSing @context of
    SReify context -> case sLabelable context of
      Dict -> case sReifiable context of
        Dict -> greify

  unreify Refl = case contextSing @context of
    SReify context -> case sLabelable context of
      Dict -> case sReifiable context of
        Dict -> gunreify


instance
  ( Rel8able t
  , Labelable from, Reifiable from
  , Labelable to, Reifiable to
  , Congruent (t from) (t to)
  )
  => Recontextualize from to (t from) (t to)


instance
  ( context ~ Expr
  , Rel8able t
  , HConstrainTable (Columns (t context)) (ConstrainDBType DBEq)
  )
  => EqTable (t context)
 where
  eqTable = hdicts @(Columns (t context)) @(ConstrainDBType DBEq)


instance
  ( context ~ Expr
  , Rel8able t
  , HConstrainTable (Columns (t context)) (ConstrainDBType DBEq)
  , HConstrainTable (Columns (t context)) (ConstrainDBType DBOrd)
  )
  => OrdTable (t context)
 where
  ordTable = hdicts @(Columns (t context)) @(ConstrainDBType DBOrd)


type instance FromExprs (t Expr) = FromExprs' t


instance
  ( x ~ t' Expr
  , result ~ Result
  , ToExprs' (Algebra t) t' t
  )
  => ToExprs x (t result)
 where
  fromResult = fromResult' @(Algebra t) @t'
  toResult = toResult' @(Algebra t) @t'


type FromExprs' :: K.Rel8able -> Type
type family FromExprs' t where
  FromExprs' (ADT t) = t Result
  FromExprs' (HKD a) = a
  FromExprs' t = t Result


type ToExprs' :: K.Algebra -> K.Rel8able -> K.Rel8able -> Constraint
class (algebra ~ Algebra t, Rel8able t') =>
  ToExprs' algebra t' t | algebra t -> t'
 where
  fromResult' :: GColumns t' (Col Result) -> t Result
  toResult' :: t Result -> GColumns t' (Col Result)


instance (Algebra t ~ 'K.Product, Rel8able t, t ~ t') =>
  ToExprs' 'K.Product t' t
 where
  fromResult' = fromColumns
  toResult' = toColumns


instance (Algebra t ~ 'K.Sum, ADTable t, t' ~ ADT t) =>
  ToExprs' 'K.Sum t' t
 where
  fromResult' = fromADT . ADT
  toResult' = (\(ADT a) -> a) . toADT
