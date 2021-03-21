{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language ViewPatterns #-}

{-# options_ghc -Wno-orphans #-}

module Rel8.Table.ListTable ( ListTable( ListTable ), ListOf ) where

-- rel8
import Rel8.Context ( Column( I ), Meta( Meta ), unI )
import Rel8.DatabaseType ( listOfNotNull, listOfNull )
import Rel8.Expr ( Expr )
import Rel8.Expr.Instances ( Column( ExprColumn, fromExprColumn ) )
import Rel8.Expr.Opaleye ( binaryOperator, litExprWith )
import Rel8.HTable ( hdbtype, hfield, htabulateMeta, htraverseMeta, hzipWith )
import Rel8.HTable.HMapTable
  ( Eval
  , Exp
  , HMapTable
  , HMapTableField( HMapTableField )
  , MapInfo( mapInfo )
  , precomposed
  , unHMapTable
  )
import Rel8.Info ( Column( InfoColumn ), Info( NotNull, Null ) )
import Rel8.Serializable ( ExprFor( pack, unpack ), Serializable )
import Rel8.Table ( Columns, Table, fromColumns, toColumns )


data ListOf :: Meta -> Exp Meta


type instance Eval (ListOf ('Meta defaulting x)) = 'Meta defaulting [x]


instance MapInfo ListOf where
  mapInfo (InfoColumn i) = InfoColumn $ case i of
    NotNull t -> NotNull $ listOfNotNull t
    Null t -> NotNull $ listOfNull t


-- | A @ListTable@ value contains zero or more instances of @a@. You construct
-- @ListTable@s with 'many' or 'listAgg'.
newtype ListTable a = ListTable (HMapTable ListOf (Columns a) (Column Expr))


instance Table Expr a => Semigroup (ListTable a) where
  ListTable a <> ListTable b =
    ListTable (hzipWith (\x y -> ExprColumn $ binaryOperator "||" (fromExprColumn x) (fromExprColumn y)) a b)


instance Table Expr a => Monoid (ListTable a) where
  mempty = ListTable $ htabulateMeta \i ->
    case hfield hdbtype i of
      InfoColumn x ->
        case i of
          HMapTableField j ->
            case hfield hdbtype j of
              InfoColumn _ ->
                ExprColumn $ litExprWith x []


instance (f ~ Expr, Table f a) => Table f (ListTable a) where
  type Columns (ListTable a) = HMapTable ListOf (Columns a)

  toColumns (ListTable a) = a
  fromColumns a = ListTable a


instance (a ~ ListTable x, Table Expr (ListTable x), ExprFor x b) => ExprFor a [b] where
  pack (unHMapTable -> xs) =
    pack @x <$> htraverseMeta (fmap I . unI . precomposed) xs

  unpack (fmap (unpack @x) -> xs) = htabulateMeta \(HMapTableField j) ->
    case hfield hdbtype j of
      InfoColumn _ ->
        I $ fmap (unI . flip hfield j) xs


instance Serializable a b => Serializable (ListTable a) [b] where
