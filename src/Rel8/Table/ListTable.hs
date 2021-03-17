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

module Rel8.Table.ListTable ( ListTable( ListTable ) ) where

-- base
import Control.Applicative ( ZipList( ZipList, getZipList ) )
import Data.Functor.Compose ( Compose( Compose, getCompose ) )

-- rel8
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import Rel8.Context ( Context )
import Rel8.DBFunctor ( DBFunctor( liftDatabaseType ) )
import Rel8.DatabaseType ( DatabaseType( typeName ) )
import Rel8.DatabaseType.Decoder ( listDecoder )
import Rel8.Expr ( Expr( toPrimExpr ), binaryOperator, fromPrimExpr )
import Rel8.Expr.Lit ( litExprWith )
import Rel8.HTable ( HTable( hdbtype, htabulate, hfield ), hzipWith )
import Rel8.HTable.HComposeTable
  ( ComposeInner( ComposeInner )
  , HComposeTable( HComposeTable )
  , zipComposeInnerWith
  )
import Rel8.Serializable ( ExprFor, Serializable( lit, rowParser ) )
import Rel8.Table ( Table( Columns, toColumns, fromColumns ) )


-- | A @ListTable@ value contains zero or more instances of @a@. You construct
-- @ListTable@s with 'many' or 'listAgg'.
newtype ListTable a = ListTable (Columns a (Context (ComposeInner (Context Expr) [])))


instance Table Expr a => Semigroup (ListTable a) where
  ListTable a <> ListTable b =
    ListTable (hzipWith (zipComposeInnerWith (binaryOperator "||")) a b)


instance Table Expr a => Monoid (ListTable a) where
  mempty = ListTable $ htabulate $ \field ->
    case hfield hdbtype field of
      databaseType -> ComposeInner $ litExprWith (liftDatabaseType databaseType) []


instance (f ~ Expr, Table f a) => Table f (ListTable a) where
  type Columns (ListTable a) = HComposeTable [] (Columns a)

  toColumns (ListTable a) = HComposeTable a
  fromColumns (HComposeTable a) = ListTable a


instance (a ~ ListTable x, Table Expr (ListTable x), ExprFor x b) => ExprFor a [b]


instance Serializable a b => Serializable (ListTable a) [b] where
  lit (map (lit @a) -> xs) = ListTable $ htabulate $ \field ->
    case hfield hdbtype field of
      databaseType -> ComposeInner $ listOfExprs databaseType $
        map (\x -> hfield (toColumns x) field) xs
    where
      listOfExprs :: DatabaseType x -> [Expr x] -> Expr [x]
      listOfExprs databaseType as = fromPrimExpr $
        Opaleye.CastExpr array $
        Opaleye.ArrayExpr (map toPrimExpr as)
        where
          array = typeName (liftDatabaseType @[] databaseType)

  rowParser liftHasqlDecoder =
    fmap getZipList . getCompose <$>
      rowParser @a (\x -> Compose <$> liftHasqlDecoder (fmap ZipList (listDecoder x)))
