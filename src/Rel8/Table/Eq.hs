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
import Data.Tuple ( Solo )
import GHC.Generics ( Rep )
import Prelude

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( (||.), (&&.) )
import Rel8.Expr.Eq ( (==.), (/=.) )
import Rel8.FCF ( Eval, Exp )
import Rel8.Generic.Record ( Record )
import Rel8.Generic.Table.Record ( GTable, GColumns, gtable )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable ( htabulateA, hfield )
import Rel8.Schema.HTable.Identity ( HIdentity( HIdentity ) )
import Rel8.Schema.Null ( Sql )
import Rel8.Table ( Table, Columns, toColumns, TColumns )
import Rel8.Type.Eq ( DBEq )


-- | The class of 'Table's that can be compared for equality. Equality on
-- tables is defined by equality of all columns all columns, so this class
-- means "all columns in a 'Table' have an instance of 'DBEq'".
type EqTable :: Type -> Constraint
class Table Expr a => EqTable a where
  eqTable :: Columns a (Dict (Sql DBEq))

  default eqTable ::
    ( GTable TEqTable TColumns (Rep (Record a))
    , Columns a ~ GColumns TColumns (Rep (Record a))
    )
    => Columns a (Dict (Sql DBEq))
  eqTable = gtable @TEqTable @TColumns @(Rep (Record a)) table
    where
      table (_ :: proxy x) = eqTable @x


data TEqTable :: Type -> Exp Constraint
type instance Eval (TEqTable a) = EqTable a


instance Sql DBEq a => EqTable (Expr a) where
  eqTable = HIdentity Dict


instance EqTable a => EqTable (Solo a) where
  eqTable = eqTable @a


instance (EqTable a, EqTable b) => EqTable (a, b)


instance (EqTable a, EqTable b, EqTable c) => EqTable (a, b, c)


instance (EqTable a, EqTable b, EqTable c, EqTable d) => EqTable (a, b, c, d)


instance (EqTable a, EqTable b, EqTable c, EqTable d, EqTable e) =>
  EqTable (a, b, c, d, e)


instance (EqTable a, EqTable b, EqTable c, EqTable d, EqTable e, EqTable f) =>
  EqTable (a, b, c, d, e, f)


instance
  ( EqTable a, EqTable b, EqTable c, EqTable d, EqTable e, EqTable f
  , EqTable g
  )
  => EqTable (a, b, c, d, e, f, g)


-- | Compare two 'Table's for equality. This corresponds to comparing all
-- columns inside each table for equality, and combining all comparisons with
-- @AND@.
(==:) :: forall a. EqTable a => a -> a -> Expr Bool
(toColumns -> as) ==: (toColumns -> bs) =
  foldl1' (&&.) $ getConst $ htabulateA $ \field ->
    case (hfield as field, hfield bs field) of
      (a, b) -> case hfield (eqTable @a) field of
        Dict -> Const (pure (a ==. b))
infix 4 ==:


-- | Test if two 'Table's are different. This corresponds to comparing all
-- columns inside each table for inequality, and combining all comparisons with
-- @OR@.
(/=:) :: forall a. EqTable a => a -> a -> Expr Bool
(toColumns -> as) /=: (toColumns -> bs) =
  foldl1' (||.) $ getConst $ htabulateA $ \field ->
    case (hfield as field, hfield bs field) of
      (a, b) -> case hfield (eqTable @a) field of
        Dict -> Const (pure (a /=. b))
infix 4 /=:


foldl1' :: (a -> a -> a) -> NonEmpty a -> a
foldl1' f (a :| as) = foldl' f a as
