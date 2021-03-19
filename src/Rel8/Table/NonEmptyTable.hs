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
import Data.Functor.Identity ( Identity( runIdentity ) )
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty, toList )
import qualified Data.List.NonEmpty as NonEmpty

-- rel8
import Rel8.Context ( Context )
import Rel8.DatabaseType ( nonEmptyNotNull, nonEmptyNull )
import Rel8.Expr ( Expr, binaryOperator )
import Rel8.HTable ( HTable( hfield, htabulate, htraverse ), hzipWith )
import Rel8.HTable.HMapTable ( Eval, Exp, HMapTable, HMapTableField( HMapTableField ), MapInfo( mapInfo ), precomposed, unHMapTable )
import Rel8.Info ( Info( NotNull, Null ) )
import Rel8.Serializable ( ExprFor( pack, unpack ), Serializable )
import Rel8.Table ( Table( Columns, fromColumns, toColumns ) )


data NonEmptyList :: Type -> Exp Type


type instance Eval (NonEmptyList a) = NonEmpty a


instance MapInfo NonEmptyList where
  mapInfo = \case
    NotNull t -> NotNull $ nonEmptyNotNull t
    Null t -> NotNull $ nonEmptyNull t


-- | A @NonEmptyTable@ value contains one or more instances of @a@. You
-- construct @NonEmptyTable@s with 'some' or 'nonEmptyAgg'.
newtype NonEmptyTable a = NonEmptyTable (HMapTable NonEmptyList (Columns a) (Context Expr))


instance (f ~ Expr, Table f a) => Table f (NonEmptyTable a) where
  type Columns (NonEmptyTable a) = HMapTable NonEmptyList (Columns a)

  toColumns (NonEmptyTable a) = a
  fromColumns = NonEmptyTable


instance (Serializable x b, a ~ NonEmptyTable x, Table Expr (NonEmptyTable x)) => ExprFor a (NonEmpty b) where
  pack (unHMapTable -> xs) = NonEmpty.fromList $ getZipList $ pack @x <$> htraverse (fmap pure . ZipList . toList . runIdentity . precomposed) xs

  unpack (fmap (unpack @x) -> xs) = htabulate \(HMapTableField i) ->
    pure (fmap (runIdentity . flip hfield i) xs)


instance Serializable a b => Serializable (NonEmptyTable a) (NonEmpty b) where


instance Table Expr a => Semigroup (NonEmptyTable a) where
  NonEmptyTable a <> NonEmptyTable b =
    NonEmptyTable (hzipWith (binaryOperator "||") a b)
