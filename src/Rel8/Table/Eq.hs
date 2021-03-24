{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
{-# language ViewPatterns #-}

module Rel8.Table.Eq
  ( EqTable, (==:), (/=:)
  )
where

-- base
import Data.Foldable ( foldl' )
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Prelude hiding ( seq )

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( (||.), (&&.) )
import Rel8.Expr.Eq ( seq, sne )
import Rel8.Opaque ( Opaque )
import Rel8.Schema.Context ( Col'(..) )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable
  ( HConstrainTable
  , htabulateA, hfield
  , hdicts, hspecs
  )
import Rel8.Schema.Spec ( SSpec(..) )
import Rel8.Schema.Spec.ConstrainDBType ( ConstrainDBType )
import Rel8.Table ( Table, Columns, toColumns )
import Rel8.Type.Eq ( DBEq )


type EqTable :: Type -> Constraint
class
  ( Table Expr a
  , HConstrainTable (Columns a) (ConstrainDBType DBEq)
  ) => EqTable a
instance
  ( Table Expr a
  , HConstrainTable (Columns a) (ConstrainDBType DBEq)
  ) => EqTable a
instance {-# OVERLAPPING #-} EqTable Opaque


-- | Compare two 'Table's for equality. This corresponds to comparing all
-- columns inside each table for equality, and combining all comparisons with
-- @AND@.
(==:) :: forall a. EqTable a => a -> a -> Expr Bool
(toColumns -> as) ==: (toColumns -> bs) =
  foldl1' (&&.) $ getConst $ htabulateA $ \field ->
    case (hfield as field, hfield bs field) of
      (DB a, DB b) -> case hfield dicts field of
        Dict -> case hfield specs field of
          SSpec {nullability} -> Const (pure (seq nullability a b))
  where
    dicts = hdicts @(Columns a) @(ConstrainDBType DBEq)
    specs = hspecs @(Columns a)
infix 4 ==:


-- | Test if two 'Table's are different. This corresponds to comparing all
-- columns inside each table for inequality, and combining all comparisons with
-- @OR@.
(/=:) :: forall a. EqTable a => a -> a -> Expr Bool
(toColumns -> as) /=: (toColumns -> bs) =
  foldl1' (||.) $ getConst $ htabulateA $ \field ->
    case (hfield as field, hfield bs field) of
      (DB a, DB b) -> case hfield dicts field of
        Dict -> case hfield specs field of
          SSpec {nullability} -> Const (pure (sne nullability a b))
  where
    dicts = hdicts @(Columns a) @(ConstrainDBType DBEq)
    specs = hspecs @(Columns a)
infix 4 /=:


foldl1' :: (a -> a -> a) -> NonEmpty a -> a
foldl1' f (a :| as) = foldl' f a as
