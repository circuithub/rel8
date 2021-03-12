{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
{-# language ViewPatterns #-}

module Rel8.Table.Ord
  ( OrdTable, (<:), (<=:), (>:), (>=:), least, greatest
  )
where

-- base
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( (||.), (&&.), false, true )
import Rel8.Expr.Eq ( (==.) )
import Rel8.Expr.Ord ( (<.), (>.) )
import Rel8.Kind.Nullability
  ( Nullability( NonNullable )
  , withKnownNullability
  )
import Rel8.Schema.Context ( DB( DB ) )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable
  ( HConstrainTable
  , htabulateA, hfield
  , hdicts, hspecs
  )
import Rel8.Schema.Spec ( SSpec( SSpec ) )
import Rel8.Schema.Spec.ConstrainType ( ConstrainType )
import Rel8.Table ( Columns, toColumns )
import Rel8.Table.Bool ( bool )
import Rel8.Table.Eq ( EqTable )
import Rel8.Type.Ord ( DBOrd )


type OrdTable :: Type -> Constraint
class
  ( EqTable a
  , HConstrainTable (Columns a) (ConstrainType DBOrd)
  ) => OrdTable a
instance
  ( EqTable a
  , HConstrainTable (Columns a) (ConstrainType DBOrd)
  ) => OrdTable a


(<:) :: forall a. OrdTable a => a -> a -> Expr 'NonNullable Bool
(toColumns -> as) <: (toColumns -> bs) =
  foldr @[] go false $ getConst $ htabulateA $ \field ->
    case (hfield as field, hfield bs field) of
      (DB a, DB b) -> case hfield dicts field of
        Dict -> case hfield specs field of
          SSpec _ nullability _ -> withKnownNullability nullability $
            Const [(a <. b, a ==. b)]
  where
    dicts = hdicts @(Columns a) @(ConstrainType DBOrd)
    specs = hspecs @(Columns a)
    go (lt, eq) a = lt ||. (eq &&. a)
infix 4 <:


(<=:) :: forall a. OrdTable a => a -> a -> Expr 'NonNullable Bool
(toColumns -> as) <=: (toColumns -> bs) =
  foldr @[] go true $ getConst $ htabulateA $ \field ->
    case (hfield as field, hfield bs field) of
      (DB a, DB b) -> case hfield dicts field of
        Dict -> case hfield specs field of
          SSpec _ nullability _ -> withKnownNullability nullability $
            Const [(a <. b, a ==. b)]
  where
    dicts = hdicts @(Columns a) @(ConstrainType DBOrd)
    specs = hspecs @(Columns a)
    go (lt, eq) a = lt ||. (eq &&. a)
infix 4 <=:


(>:) :: forall a. OrdTable a => a -> a -> Expr 'NonNullable Bool
(toColumns -> as) >: (toColumns -> bs) =
  foldr @[] go false $ getConst $ htabulateA $ \field ->
    case (hfield as field, hfield bs field) of
      (DB a, DB b) -> case hfield dicts field of
        Dict -> case hfield specs field of
          SSpec _ nullability _ -> withKnownNullability nullability $
            Const [(a >. b, a ==. b)]
  where
    dicts = hdicts @(Columns a) @(ConstrainType DBOrd)
    specs = hspecs @(Columns a)
    go (gt, eq) a = gt ||. (eq &&. a)
infix 4 >:


(>=:) :: forall a. OrdTable a => a -> a -> Expr 'NonNullable Bool
(toColumns -> as) >=: (toColumns -> bs) =
  foldr @[] go true $ getConst $ htabulateA $ \field ->
    case (hfield as field, hfield bs field) of
      (DB a, DB b) -> case hfield dicts field of
        Dict -> case hfield specs field of
          SSpec _ nullability _ -> withKnownNullability nullability $
            Const [(a >. b, a ==. b)]
  where
    dicts = hdicts @(Columns a) @(ConstrainType DBOrd)
    specs = hspecs @(Columns a)
    go (gt, eq) a = gt ||. (eq &&. a)
infix 4 >=:


least :: OrdTable a => a -> a -> a
least a b = bool a b (a <: b)


greatest :: OrdTable a => a -> a -> a
greatest a b = bool a b (a >: b)
