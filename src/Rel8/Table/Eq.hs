{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
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
import GHC.Generics ( Rep, (:*:), K1, M1, Meta( MetaSel ), D, C, S )
import GHC.TypeLits ( KnownSymbol )
import Prelude

-- rel8
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Expr.Bool ( (||.), (&&.) )
import Rel8.Expr.Eq ( (==.), (/=.) )
import Rel8.Generic.Record ( Record )
import Rel8.Schema.Context.Label ( hlabeler )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable
  ( HTable, HConstrainTable
  , htabulateA, hfield, hdicts
  )
import Rel8.Schema.HTable.Label ( HLabel, hlabel )
import Rel8.Schema.HTable.Product ( HProduct(..) )
import Rel8.Schema.HTable.Type ( HType(..) )
import Rel8.Schema.Kind ( Context )
import Rel8.Schema.Null ( Sql )
import Rel8.Schema.Spec.ConstrainDBType ( ConstrainDBType )
import Rel8.Table ( Table, Columns, toColumns, GColumns )
import Rel8.Type.Eq ( DBEq )


-- | The class of 'Table's that can be compared for equality. Equality on
-- tables is defined by equality of all columns all columns, so this class
-- means "all columns in a 'Table' have an instance of 'DBEq'".
type EqTable :: Type -> Constraint
class Table Expr a => EqTable a where
  eqTable :: Columns a (Dict (ConstrainDBType DBEq))

  default eqTable ::
    ( GColumns (Rep (Record a)) ~ Columns a
    , GEqTable (Rep (Record a))
    )
    => Columns a (Dict (ConstrainDBType DBEq))
  eqTable = geqTable @(Rep (Record a))


type GEqTable :: (Type -> Type) -> Constraint
class GEqTable rep where
  geqTable :: GColumns rep (Dict (ConstrainDBType DBEq))


instance GEqTable rep => GEqTable (M1 D c rep) where
  geqTable = geqTable @rep


instance GEqTable rep => GEqTable (M1 C c rep) where
  geqTable = geqTable @rep


instance (GEqTable rep1, GEqTable rep2) => GEqTable (rep1 :*: rep2) where
  geqTable = HProduct (geqTable @rep1) (geqTable @rep2)


instance
  ( EqTable a
  , KnownSymbol label
  , GColumns (M1 S meta k1) ~ HLabel label (Columns a)
  , meta ~ 'MetaSel ('Just label) _su _ss _ds
  , k1 ~ K1 i a
  ) => GEqTable (M1 S meta k1)
 where
  geqTable = hlabel hlabeler (eqTable @a)


instance
  ( Table Expr (t (Expr :: Context))
  , f ~ Expr
  , HConstrainTable (Columns (t Expr)) (ConstrainDBType DBEq)
  )
  => EqTable (t f)
 where
  eqTable = hdicts @(Columns (t f)) @(ConstrainDBType DBEq)


instance
  ( HTable t
  , f ~ Col Expr
  , HConstrainTable t (ConstrainDBType DBEq)
  )
  => EqTable (t f)
 where
  eqTable = hdicts @(Columns (t f)) @(ConstrainDBType DBEq)


instance Sql DBEq a => EqTable (Expr a) where
  eqTable = HType Dict


instance (EqTable a, EqTable b) => EqTable (a, b)


instance (EqTable a, EqTable b, EqTable c) => EqTable (a, b, c)


instance (EqTable a, EqTable b, EqTable c, EqTable d) => EqTable (a, b, c, d)


instance (EqTable a, EqTable b, EqTable c, EqTable d, EqTable e) =>
  EqTable (a, b, c, d, e)


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
