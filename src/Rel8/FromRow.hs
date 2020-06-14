{-# language BlockArguments #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.FromRow where

import Control.Applicative ( liftA2 )
import Rel8.MaybeTable
import Data.Functor.Identity
import Data.Int
import Database.PostgreSQL.Simple.FromField ( FromField )
import Database.PostgreSQL.Simple.FromRow ( RowParser, field )
import Rel8.Column
import Rel8.Expr
import Rel8.HigherKindedTable
import Rel8.Query
import Rel8.Table hiding ( field )


-- | @FromRow@ witnesses the one-to-one correspondence between the type @sql@,
-- which contains SQL expressions, and the type @haskell@, which contains the
-- Haskell decoding of rows containing @sql@ SQL expressions.
class ( Context sql ~ Expr, Table sql ) => FromRow sql haskell | sql -> haskell, haskell -> sql where
  rowParser :: sql -> RowParser haskell


-- | Any higher-kinded records can be @SELECT@ed, as long as we know how to
-- decode all of the records constituent part's.
instance ( HConstrainTable t Identity FromField, HConstrainTable t Identity Unconstrained, HigherKindedTable t, Table ( t expr ), expr ~ Expr, identity ~ Identity ) => FromRow ( t expr ) ( t identity ) where
  rowParser =
    traverseTableC @Select @FromField ( traverseCC @FromField \_ -> field )


instance m ~ Query => FromRow ( Expr Int32 ) Int32 where
  rowParser _ =
    field


instance m ~ Query => FromRow ( Expr Int64 ) Int64 where
  rowParser _ =
    field


instance m ~ Query => FromRow ( Expr Bool ) Bool where
  rowParser _ =
    field


instance m ~ Query => FromRow ( Expr ( Maybe Int64 ) ) ( Maybe Int64 ) where
  rowParser _ =
    field


instance (FromRow a1 b1, FromRow a2 b2) => FromRow (a1, a2) (b1, b2) where
  rowParser (a, b) =
    liftA2 (,) (rowParser a) (rowParser b)


instance
  ( f ~ Expr
  , g ~ Identity
  , HConstrainTable t Expr Unconstrained
  , HConstrainTable t Expr FromField
  , HConstrainTable t Identity FromField
  , HigherKindedTable t
  , Table ( MaybeTable ( t f ) )
  , Table ( t g )
  ) => FromRow ( MaybeTable ( t f ) ) ( Maybe ( t g ) ) where

  rowParser ( MaybeTable _ t ) = do
    rowExists <- field

    case rowExists of
      Just True ->
        Just <$> rowParser t

      _ ->
        Nothing <$ traverseTableC @Id @FromField nullField t


nullField :: forall x f. FromField x => C f x -> RowParser ( C f x )
nullField x = x <$ field @( Maybe x )
