{-# language BlockArguments #-}
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

module Rel8.Table.NonEmptyTable ( NonEmptyTable(..) ) where

-- base
import Control.Applicative ( ZipList( ZipList, getZipList ) )
import Data.Functor.Compose ( Compose( Compose, getCompose ) )
import Data.List.NonEmpty ( NonEmpty, toList )
import qualified Data.List.NonEmpty as NonEmpty

-- rel8
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Context ( Context )
import Rel8.DBFunctor ( DBFunctor( liftDatabaseType ) )
import Rel8.DatabaseType ( DatabaseType( typeName ) )
import Rel8.DatabaseType.Decoder ( listDecoder )
import Rel8.Expr ( Expr( toPrimExpr ), binaryOperator, fromPrimExpr )
import Rel8.HTable ( HTable( hfield, htabulate, hdbtype ), hzipWith )
import Rel8.HTable.HComposeTable
  ( ComposeInner( ComposeInner )
  , HComposeTable( HComposeTable )
  , zipComposeInnerWith
  )
import Rel8.Serializable ( ExprFor, Serializable( rowParser, lit ) )
import Rel8.Table ( Table( Columns, fromColumns, toColumns ) )


-- | A @NonEmptyTable@ value contains one or more instances of @a@. You
-- construct @NonEmptyTable@s with 'some' or 'nonEmptyAgg'.
newtype NonEmptyTable a = NonEmptyTable (Columns a (Context (ComposeInner (Context Expr) NonEmpty)))


instance (f ~ Expr, Table f a) => Table f (NonEmptyTable a) where
  type Columns (NonEmptyTable a) = HComposeTable NonEmpty (Columns a)

  toColumns (NonEmptyTable a) = HComposeTable a
  fromColumns (HComposeTable a) = NonEmptyTable a


instance (a ~ NonEmptyTable x, Table Expr (NonEmptyTable x), ExprFor x b)                   => ExprFor a                (NonEmpty b)


instance Serializable a b => Serializable (NonEmptyTable a) (NonEmpty b) where
  lit (fmap (lit @a) -> xs) = NonEmptyTable $ htabulate $ \field ->
    case hfield hdbtype field of
      databaseType -> ComposeInner $ nonEmptyOf databaseType $
        fmap (\x -> hfield (toColumns x) field) xs
    where
      nonEmptyOf :: DatabaseType x -> NonEmpty (Expr x) -> Expr (NonEmpty x)
      nonEmptyOf databaseType as = fromPrimExpr $
        Opaleye.CastExpr array $
        Opaleye.ArrayExpr (fmap toPrimExpr (toList as))
        where
          array = typeName (liftDatabaseType @NonEmpty databaseType)

  rowParser liftHasqlDecoder =
    fmap (NonEmpty.fromList . getZipList) . getCompose <$>
      rowParser @a (\x -> Compose <$> liftHasqlDecoder (fmap ZipList (listDecoder x)))


instance Table Expr a => Semigroup (NonEmptyTable a) where
  NonEmptyTable a <> NonEmptyTable b =
    NonEmptyTable (hzipWith (zipComposeInnerWith (binaryOperator "||")) a b)
