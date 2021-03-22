{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Table.EqTable ( EqTable, (==:) ) where

-- base
import Control.Applicative ( Const( Const ), getConst )
import Data.Functor.Compose ( Compose )

-- rel8
import Rel8.Context ( Column( ComposedColumn ), decompose )
import Rel8.DBType.DBEq ( DBEq( (==.) ) )
import Rel8.Expr ( Expr )
import Rel8.Expr.Bool ( and_ )
import Rel8.Expr.Instances ( Column( ExprColumn ) )
import Rel8.HTable ( ColType, Column( DictColumn ), HField, hdict, hfield, htabulate, htraverse, refine )
import Rel8.Table ( AllColumns, Columns, Table, toColumns )


class (Table Expr t, AllColumns t DBEq) => EqTable t


instance (Table Expr t, AllColumns t DBEq) => EqTable t


(==:) :: forall a. (Table Expr a, AllColumns a DBEq) => a -> a -> Expr Bool
a ==: b = and_ $ getConst $ htraverse decompose $ htabulate f
  where
    f :: HField (Columns a) x -> Column (Compose Expr (Const [Expr Bool])) x
    f i =
      case hfield (hdict @_ @(ColType DBEq)) i of
        DictColumn ->
          refine @DBEq i $
            case (hfield (toColumns a) i, hfield (toColumns b) i) of
              (ExprColumn x, ExprColumn y) ->
                ComposedColumn $ Const [ x ==. y ]
