{-# language AllowAmbiguousTypes #-}
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

module Rel8.Table.Ord
  ( OrdTable( ordTable ), (<:), (<=:), (>:), (>=:), least, greatest
  )
where

-- base
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Kind ( Constraint, Type )
import GHC.Generics ( Rep )
import Prelude hiding ( seq )

-- rel8
import Rel8.Expr ( Expr, Col( E ) )
import Rel8.Expr.Bool ( (||.), (&&.), false, true )
import Rel8.Expr.Eq ( (==.) )
import Rel8.Expr.Ord ( (<.), (>.) )
import Rel8.FCF ( Eval, Exp )
import Rel8.Generic.Record ( Record )
import Rel8.Generic.Table.Record ( GTable, GColumns, gtable )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable
  ( HTable, HConstrainTable
  , htabulateA, hfield, hdicts
  )
import Rel8.Schema.HTable.Identity ( HIdentity( HType ) )
import Rel8.Schema.Null (Sql)
import Rel8.Schema.Spec.Constrain ( ConstrainSpec )
import Rel8.Table ( Columns, toColumns, TColumns, TFromExprs )
import Rel8.Table.Bool ( bool )
import Rel8.Table.Eq ( EqTable )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Ord ( DBOrd )


-- | The class of 'Table's that can be ordered. Ordering on tables is defined
-- by their lexicographic ordering of all columns, so this class means "all
-- columns in a 'Table' have an instance of 'DBOrd'".
type OrdTable :: Type -> Constraint
class EqTable a => OrdTable a where
  ordTable :: Columns a (Dict (ConstrainSpec (Sql DBOrd)))

  default ordTable ::
    ( GTable TOrdTable TColumns TFromExprs (Rep (Record a))
    , Columns a ~ GColumns TColumns (Rep (Record a))
    )
    => Columns a (Dict (ConstrainSpec (Sql DBOrd)))
  ordTable =
    gtable
      @TOrdTable
      @TColumns
      @TFromExprs
      @(Rep (Record a))
      table
    where
      table (_ :: proxy x) = ordTable @x


data TOrdTable :: Type -> Exp Constraint
type instance Eval (TOrdTable a) = OrdTable a


instance
  ( HTable t
  , f ~ Col Expr
  , HConstrainTable t (ConstrainSpec (Sql DBEq))
  , HConstrainTable t (ConstrainSpec (Sql DBOrd))
  )
  => OrdTable (t f)
 where
  ordTable = hdicts @(Columns (t f)) @(ConstrainSpec (Sql DBOrd))


instance Sql DBOrd a => OrdTable (Expr a) where
  ordTable = HType Dict


instance (OrdTable a, OrdTable b) => OrdTable (a, b)


instance (OrdTable a, OrdTable b, OrdTable c) => OrdTable (a, b, c)


instance (OrdTable a, OrdTable b, OrdTable c, OrdTable d) => OrdTable (a, b, c, d)


instance (OrdTable a, OrdTable b, OrdTable c, OrdTable d, OrdTable e) =>
  OrdTable (a, b, c, d, e)


instance
  ( OrdTable a, OrdTable b, OrdTable c, OrdTable d, OrdTable e, OrdTable f
  )
  => OrdTable (a, b, c, d, e, f)


instance
  ( OrdTable a, OrdTable b, OrdTable c, OrdTable d, OrdTable e, OrdTable f
  , OrdTable g
  )
  => OrdTable (a, b, c, d, e, f, g)


-- | Test if one 'Table' sorts before another. Corresponds to comparing all
-- columns with '<'.
(<:) :: forall a. OrdTable a => a -> a -> Expr Bool
(toColumns -> as) <: (toColumns -> bs) =
  foldr @[] go false $ getConst $ htabulateA $ \field ->
    case (hfield as field, hfield bs field) of
      (E a, E b) -> case hfield (ordTable @a) field of
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
      (E a, E b) -> case hfield (ordTable @a) field of
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
      (E a, E b) -> case hfield (ordTable @a) field of
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
      (E a, E b) -> case hfield (ordTable @a) field of
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
