{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
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
import Rel8.Context ( Column( I ), Meta( Meta ), unI )
import Rel8.DBType ( Column( InfoColumn ), Info( NotNull, Null ) )
import Rel8.DatabaseType ( nonEmptyNotNull, nonEmptyNull )
import Rel8.Expr ( Expr )
import Rel8.Expr.Instances ( Column( ExprColumn ), fromExprColumn )
import Rel8.Expr.Opaleye ( binaryOperator )
import Rel8.HTable ( HTable, hdbtype, hfield, htabulateMeta, htraverseMeta, hzipWith )
import Rel8.HTable.HMapTable ( Eval, Exp, HMapTable, HMapTableField( HMapTableField ), MapInfo( mapInfo ), precomposed, unHMapTable )
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


instance (HTable (Columns (NonEmptyTable a)), f ~ Expr, Table f a) => Table f (NonEmptyTable a) where
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


instance (Table Expr (NonEmptyTable a), Serializable a b) => Serializable (NonEmptyTable a) (NonEmpty b) where


instance (Table Expr (NonEmptyTable a), Table Expr a) => Semigroup (NonEmptyTable a) where
  NonEmptyTable a <> NonEmptyTable b =
    NonEmptyTable (hzipWith (\x y -> ExprColumn $ binaryOperator "||" (fromExprColumn x) (fromExprColumn y)) a b)
