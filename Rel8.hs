{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8 where

import Control.Monad.Trans.State.Strict
import Data.String
import Numeric.Natural ( Natural )
import qualified SQL
import HList


class Table ( t :: ( * -> * ) -> * ) where
  type Schema t :: [ * ]

  toHList :: t f -> HList f ( Schema t )

  fromHList :: HList f ( Schema t ) -> t f


mapTable
  :: Table table
  => ( forall x. f x -> g x ) -> table f -> table g
mapTable f =
  fromHList . hmap f . toHList


data ColumnDefinition a where
  ColumnDefinition :: String -> ColumnDefinition a


instance IsString ( ColumnDefinition a ) where
  fromString =
    ColumnDefinition


data TableSchema (t :: (* -> *) -> *) where
  TableSchema
    :: Table t
    => { tableName :: String
       , tableSchema :: t ColumnDefinition
       }
    -> TableSchema t


data Expr (s :: *) (a :: *) where
  Expr :: SQL.Expr -> Expr s a


newtype Query a = Query ( State ( SQL.Query () ) a )
  deriving newtype ( Functor, Applicative, Monad )


table :: TableSchema t -> Query (t (Expr s))
table TableSchema{ tableName, tableSchema } =
  Query do
    modify do
      SQL.Product ( SQL.SelectTable tableName "TODO" )

    return
      ( mapTable
          ( \( ColumnDefinition s ) ->
              Expr s
          )
          tableSchema
      )


where_ :: Expr s Bool -> Query ()
where_ ( Expr e ) =
  Query ( modify ( SQL.Where e ) )


newtype Order s =
  Order { toExprs :: [ SQL.Expr ] }


orderBy
  :: (forall s. Query (t (Expr s)) -> Order s)
  -> Query (t (Expr s))
  -> Query (t (Expr s))
orderBy f ( Query q ) = Query do
  let
    ( a, sql ) =
      runState q ( SQL.One () )

  modify
    ( SQL.Product
        ( SQL.Order
            ( toExprs ( f ( Query q ) ) )
            sql
        )
    )

  return a


limit :: Natural -> Query a -> Query a
limit n ( Query q ) = Query do
  let
    ( a, sql ) =
      runState q ( SQL.One () )

  modify ( SQL.Product ( SQL.Limit n sql ) )

  return a


offset :: Natural -> Query t -> Query t
offset n ( Query q ) = Query do
  let
    ( a, sql ) =
      runState q ( SQL.One () )

  modify ( SQL.Product ( SQL.Limit n sql ) )

  return a


class Select ( table :: ( * -> * ) -> * ) b | table -> b where


select
  :: ( Table t, Select t row )
  => ( forall s. Query ( t ( Expr s ) ) ) -> IO [ row ]
select ( Query q ) = do
  let
    ( table, sql ) =
      runState q ( SQL.One () )

    projections =
      collect ( \( Expr e ) -> e ) ( toHList table )

  print
    ( SQL.renderQuery
        <$> SQL.withoutOne
              ( SQL.Project
                  ( map ( \e -> ( e, "" ) ) projections )
                  sql
              )
    )

  undefined


newtype C a f =
  C ( f a )


instance a ~ b => Select ( C a ) b where



instance IsString ( f a ) => IsString ( C a f ) where
  fromString =
    C . fromString


instance Table ( C a ) where
  type Schema ( C a ) = '[ a ]

  toHList ( C a ) = HCons a HNil

  fromHList ( HCons a HNil ) =
    C a


data (&) t1 t2 ( f :: * -> * ) =
  (:&) ( t1 f ) ( t2 f )


instance ( Table t1, Table t2, SList ( Schema t1 ) ) => Table ( (&) t1 t2 ) where
  type Schema ( t1 & t2 ) = Schema t1 ++ Schema t2

  toHList ( t1 :& t2 ) =
    happend ( toHList t1 ) ( toHList t2 )

  fromHList cols =
    case split cols of
      ( cols1, cols2 ) ->
        fromHList cols1 :& fromHList cols2


instance ( Select t1 a, Select t2 b ) => Select ( t1 & t2 ) ( a, b )
