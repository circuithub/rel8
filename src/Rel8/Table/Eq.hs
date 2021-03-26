{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
{-# language ViewPatterns #-}

module Rel8.Table.Eq
  ( EqTable( eqTable ), (==:), (/=:)
  )
where

-- base
import Data.Foldable ( foldl' )
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Prelude

-- rel8
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Expr.Bool ( (||.), (&&.) )
import Rel8.Expr.Eq ( (==.), (/=.) )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( HConstrainTable, HPairField(..), GHField(..), htabulate, htabulateA, hfield, hdicts, hspecs )
import Rel8.Schema.Spec.ConstrainDBType ( ConstrainDBType )
import Rel8.Table ( Table, Columns, toColumns )
import Rel8.Type.Eq ( DBEq )
import Rel8.Schema.HTable.MapTable ( HMapTableField(..) )
import Rel8.Schema.Spec ( SSpec( SSpec ) )
import Rel8.Schema.Nullability ( Sql )


type EqTable :: Type -> Constraint
class Table Expr a => EqTable a where
  eqTable :: Columns a (Dict (ConstrainDBType DBEq))

  default eqTable :: HConstrainTable (Columns a) (ConstrainDBType DBEq) => Columns a (Dict (ConstrainDBType DBEq))
  eqTable = hdicts @(Columns a) @(ConstrainDBType DBEq)


instance (Table Expr (t Expr), f ~ Expr, HConstrainTable (Columns (t Expr)) (ConstrainDBType DBEq)) => EqTable (t f)


instance Sql DBEq a => EqTable (Expr a)


instance (EqTable a, EqTable b) => EqTable (a, b) where
  eqTable = htabulate \case
    HFst (GHField (HMapTableField i)) -> 
      case hfield hspecs i of
        SSpec{} -> case hfield (eqTable @a) i of Dict -> Dict

    HSnd (GHField (HMapTableField i)) -> 
      case hfield hspecs i of
        SSpec{} -> case hfield (eqTable @b) i of Dict -> Dict


-- | Compare two 'Table's for equality. This corresponds to comparing all
-- columns inside each table for equality, and combining all comparisons with
-- @AND@.
(==:) :: forall a. EqTable a => a -> a -> Expr Bool
(toColumns -> as) ==: (toColumns -> bs) =
  foldl1' (&&.) $ getConst $ htabulateA $ \field ->
    case (hfield as field, hfield bs field) of
      (DB a, DB b) -> case hfield (eqTable @a) field of
        Dict -> Const (pure (a ==. b))
infix 4 ==:


-- | Test if two 'Table's are different. This corresponds to comparing all
-- columns inside each table for inequality, and combining all comparisons with
-- @OR@.
(/=:) :: forall a. EqTable a => a -> a -> Expr Bool
(toColumns -> as) /=: (toColumns -> bs) =
  foldl1' (||.) $ getConst $ htabulateA $ \field ->
    case (hfield as field, hfield bs field) of
      (DB a, DB b) -> case hfield (eqTable @a) field of
        Dict -> Const (pure (a /=. b))
infix 4 /=:


foldl1' :: (a -> a -> a) -> NonEmpty a -> a
foldl1' f (a :| as) = foldl' f a as
