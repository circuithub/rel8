{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language ViewPatterns #-}

{-# options_ghc -Wno-orphans #-}

module Rel8.Table.NonEmptyTable ( NonEmptyTable(..), NonEmptyList ) where

-- base
import Control.Applicative ( ZipList( ZipList, getZipList ) )
import Data.List.NonEmpty ( NonEmpty, toList )
import qualified Data.List.NonEmpty as NonEmpty

-- rel8
import Rel8.Context ( Meta( Meta ), Column (I), unI )
import Rel8.DatabaseType ( nonEmptyNotNull, nonEmptyNull )
import Rel8.Expr ( Expr, binaryOperator, Column( ExprColumn ), fromExprColumn )
import Rel8.HTable ( HTable( hfield ), hzipWith, htraverseMeta, htabulateMeta, hdbtype )
import Rel8.HTable.HMapTable ( Eval, Exp, HMapTable, HMapTableField( HMapTableField ), MapInfo( mapInfo ), precomposed, unHMapTable )
import Rel8.Info ( Info( NotNull, Null ), Column (InfoColumn) )
import Rel8.Serializable ( ExprFor( pack, unpack ), Serializable )
import Rel8.Table ( Table( Columns, fromColumns, toColumns ) )


data NonEmptyList :: Meta -> Exp Meta


type instance Eval (NonEmptyList ('Meta defaulting a)) = 'Meta defaulting (NonEmpty a)


instance MapInfo NonEmptyList where
  mapInfo (InfoColumn i) = InfoColumn $ case i of
    NotNull t -> NotNull $ nonEmptyNotNull t
    Null t -> NotNull $ nonEmptyNull t


-- | A @NonEmptyTable@ value contains one or more instances of @a@. You
-- construct @NonEmptyTable@s with 'some' or 'nonEmptyAgg'.
newtype NonEmptyTable a = NonEmptyTable (HMapTable NonEmptyList (Columns a) (Column Expr))


instance (f ~ Expr, Table f a) => Table f (NonEmptyTable a) where
  type Columns (NonEmptyTable a) = HMapTable NonEmptyList (Columns a)

  toColumns (NonEmptyTable a) = a
  fromColumns = NonEmptyTable


instance (Serializable x b, a ~ NonEmptyTable x, Table Expr (NonEmptyTable x)) => ExprFor a (NonEmpty b) where
  pack (unHMapTable -> xs) = 
    NonEmpty.fromList $ 
      getZipList $ 
        pack @x <$> htraverseMeta (fmap I . ZipList . toList . unI . precomposed) xs

  unpack (fmap (unpack @x) -> xs) = htabulateMeta \(HMapTableField i) ->
    case hfield hdbtype i of
      InfoColumn _ -> 
        I (fmap (unI . flip hfield i) xs)


instance Serializable a b => Serializable (NonEmptyTable a) (NonEmpty b) where


instance Table Expr a => Semigroup (NonEmptyTable a) where
  NonEmptyTable a <> NonEmptyTable b =
    NonEmptyTable (hzipWith (\x y -> ExprColumn $ binaryOperator "||" (fromExprColumn x) (fromExprColumn y)) a b)
