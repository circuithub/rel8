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

-- base
import Data.Functor.Identity ( Identity( runIdentity ) )
import Data.Kind ( Type )

-- rel8
import Rel8.Context ( Context )
import Rel8.DatabaseType ( listOfNotNull, listOfNull )
import Rel8.Expr ( Expr, binaryOperator )
import Rel8.Expr.Opaleye ( litExprWith )
import Rel8.HTable ( HTable( hdbtype, htabulate, hfield ), htraverse, hzipWith )
import Rel8.HTable.HMapTable
  ( Eval
  , Exp
  , HMapTable
  , HMapTableField( HMapTableField )
  , MapInfo( mapInfo )
  , precomposed
  , unHMapTable
  )
import Rel8.Info ( Info( NotNull, Null ) )
import Rel8.Serializable ( ExprFor( pack, unpack ), Serializable )
import Rel8.Table ( Table( Columns, toColumns, fromColumns ) )


data ListOf :: Type -> Exp Type


type instance Eval (ListOf x) = [x]


instance MapInfo ListOf where
  mapInfo = \case
    NotNull t -> NotNull $ listOfNotNull t
    Null t -> NotNull $ listOfNull t


-- | A @ListTable@ value contains zero or more instances of @a@. You construct
-- @ListTable@s with 'many' or 'listAgg'.
newtype ListTable a = ListTable (HMapTable ListOf (Columns a) (Context Expr))


instance Table Expr a => Semigroup (ListTable a) where
  ListTable a <> ListTable b =
    ListTable (hzipWith (binaryOperator "||") a b)


instance Table Expr a => Monoid (ListTable a) where
  mempty = ListTable $ htabulate $ \i@HMapTableField {} ->
    litExprWith (hfield hdbtype i) []


instance (f ~ Expr, Table f a) => Table f (ListTable a) where
  type Columns (ListTable a) = HMapTable ListOf (Columns a)

  toColumns (ListTable a) = a
  fromColumns a = ListTable a


instance (a ~ ListTable x, Table Expr (ListTable x), ExprFor x b) => ExprFor a [b] where
  pack (unHMapTable -> xs) =
    pack @x <$> htraverse (sequenceA . precomposed) xs

  unpack (fmap (unpack @x) -> xs) = htabulate \(HMapTableField i) ->
    pure (fmap (runIdentity . flip hfield i) xs)


instance Serializable a b => Serializable (ListTable a) [b] where
