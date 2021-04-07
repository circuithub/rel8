{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language DisambiguateRecordFields #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
{-# language ViewPatterns #-}

module Rel8.Table.Ord
  ( OrdTable( ordTable ), (<:), (<=:), (>:), (>=:), least, greatest
  )
where

-- base
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Kind ( Constraint, Type )
import Prelude hiding ( seq )

-- rel8
import Rel8.Expr ( Expr, Col(..) )
import Rel8.Expr.Bool ( (||.), (&&.), false, true )
import Rel8.Expr.Eq ( (==.) )
import Rel8.Expr.Ord ( (<.), (>.) )
import Rel8.Schema.Context.Label ( hlabeler )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable
  ( HTable, HConstrainTable
  , htabulateA, hfield, hdicts
  )
import Rel8.Schema.HTable.Label ( hlabel )
import Rel8.Schema.HTable.Pair ( HPair(..) )
import Rel8.Schema.HTable.Quartet ( HQuartet(..) )
import Rel8.Schema.HTable.Quintet ( HQuintet(..) )
import Rel8.Schema.HTable.Trio ( HTrio(..) )
import Rel8.Schema.Null (Sql)
import Rel8.Schema.Spec.ConstrainDBType ( ConstrainDBType )
import Rel8.Table ( Table, Columns, toColumns )
import Rel8.Table.Bool ( bool )
import Rel8.Table.Eq ( EqTable )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Ord ( DBOrd )


-- | The class of 'Table's that can be ordered. Ordering on tables is defined
-- by their lexicographic ordering of all columns, so this class means "all
-- columns in a 'Table' have an instance of 'DBOrd'".
type OrdTable :: Type -> Constraint
class EqTable a => OrdTable a where
  ordTable :: Columns a (Dict (ConstrainDBType DBOrd))

  default ordTable :: HConstrainTable (Columns a) (ConstrainDBType DBOrd) => Columns a (Dict (ConstrainDBType DBOrd))
  ordTable = hdicts @(Columns a) @(ConstrainDBType DBOrd)


instance
  ( Table Expr (t Expr)
  , f ~ Expr
  , HConstrainTable (Columns (t Expr)) (ConstrainDBType DBEq)
  , HConstrainTable (Columns (t Expr)) (ConstrainDBType DBOrd)
  )
  => OrdTable (t f)


instance
  ( HTable t
  , f ~ Col Expr
  , HConstrainTable t (ConstrainDBType DBEq)
  , HConstrainTable t (ConstrainDBType DBOrd)
  )
  => OrdTable (t f)


instance Sql DBOrd a => OrdTable (Expr a)


instance (OrdTable a, OrdTable b) => OrdTable (a, b) where
  ordTable =
    HPair
      { hfst = hlabel hlabeler (ordTable @a)
      , hsnd = hlabel hlabeler (ordTable @b)
      }


instance (OrdTable a, OrdTable b, OrdTable c) => OrdTable (a, b, c) where
  ordTable =
    HTrio
      { hfst = hlabel hlabeler (ordTable @a)
      , hsnd = hlabel hlabeler (ordTable @b)
      , htrd = hlabel hlabeler (ordTable @c)
      }


instance (OrdTable a, OrdTable b, OrdTable c, OrdTable d) => OrdTable (a, b, c, d)
 where
  ordTable =
    HQuartet
      { hfst = hlabel hlabeler (ordTable @a)
      , hsnd = hlabel hlabeler (ordTable @b)
      , htrd = hlabel hlabeler (ordTable @c)
      , hfrt = hlabel hlabeler (ordTable @d)
      }


instance (OrdTable a, OrdTable b, OrdTable c, OrdTable d, OrdTable e) =>
  OrdTable (a, b, c, d, e)
 where
  ordTable =
    HQuintet
      { hfst = hlabel hlabeler (ordTable @a)
      , hsnd = hlabel hlabeler (ordTable @b)
      , htrd = hlabel hlabeler (ordTable @c)
      , hfrt = hlabel hlabeler (ordTable @d)
      , hfft = hlabel hlabeler (ordTable @e)
      }


-- | Test if one 'Table' sorts before another. Corresponds to comparing all
-- columns with '<'.
(<:) :: forall a. OrdTable a => a -> a -> Expr Bool
(toColumns -> as) <: (toColumns -> bs) =
  foldr @[] go false $ getConst $ htabulateA $ \field ->
    case (hfield as field, hfield bs field) of
      (DB a, DB b) -> case hfield (ordTable @a) field of
        Dict -> Const [(a <. b, a ==. b)]
  where
    go (lt, eq) a = lt ||. (eq &&. a)
infix 4 <:


-- | Test if one 'Table' sorts before, or is equal to, another. Corresponds to
-- comparing all columns with '<='.
(<=:) :: forall a. OrdTable a => a -> a -> Expr Bool
(toColumns -> as) <=: (toColumns -> bs) =
  foldr @[] go true $ getConst $ htabulateA $ \field ->
    case (hfield as field, hfield bs field) of
      (DB a, DB b) -> case hfield (ordTable @a) field of
        Dict -> Const [(a <. b, a ==. b)]
  where
    go (lt, eq) a = lt ||. (eq &&. a)
infix 4 <=:


-- | Test if one 'Table' sorts after another. Corresponds to comparing all
-- columns with '>'.
(>:) :: forall a. OrdTable a => a -> a -> Expr Bool
(toColumns -> as) >: (toColumns -> bs) =
  foldr @[] go false $ getConst $ htabulateA $ \field ->
    case (hfield as field, hfield bs field) of
      (DB a, DB b) -> case hfield (ordTable @a) field of
        Dict -> Const [(a >. b, a ==. b)]
  where
    go (gt, eq) a = gt ||. (eq &&. a)
infix 4 >:


-- | Test if one 'Table' sorts after another. Corresponds to comparing all
-- columns with '>='.
(>=:) :: forall a. OrdTable a => a -> a -> Expr Bool
(toColumns -> as) >=: (toColumns -> bs) =
  foldr @[] go true $ getConst $ htabulateA $ \field ->
    case (hfield as field, hfield bs field) of
      (DB a, DB b) -> case hfield (ordTable @a) field of
        Dict -> Const [(a >. b, a ==. b)]
  where
    go (gt, eq) a = gt ||. (eq &&. a)
infix 4 >=:


-- | Given two 'Table's, return the table that sorts before the other.
least :: OrdTable a => a -> a -> a
least a b = bool a b (a <: b)


-- | Given two 'Table's, return the table that sorts after the other.
greatest :: OrdTable a => a -> a -> a
greatest a b = bool a b (a >: b)
