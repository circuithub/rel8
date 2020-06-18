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
import Data.Functor.Identity
import Data.Int
import Data.Text ( Text )
import Database.PostgreSQL.Simple.FromRow ( RowParser, fieldWith )
import Rel8.Column
import Rel8.Expr
import Rel8.MaybeTable
import Rel8.Query
import Rel8.Table
import Rel8.Unconstrained


-- | @FromRow@ witnesses the one-to-one correspondence between the type @sql@,
-- which contains SQL expressions, and the type @haskell@, which contains the
-- Haskell decoding of rows containing @sql@ SQL expressions.
class ExprTable sql => FromRow sql haskell | sql -> haskell, haskell -> sql where
  rowParser :: sql -> RowParser haskell


-- | Any higher-kinded records can be @SELECT@ed, as long as we know how to
-- decode all of the records constituent part's.
instance (expr ~ Expr, identity ~ Identity, ExprTable (t expr), Table (t identity), HConstrainTable t Identity DBType) => FromRow ( t expr ) ( t identity ) where
  rowParser =
    traverseTableC @DBType ( traverseCC @DBType \_ -> fieldWith ( decode typeInformation ) )


instance m ~ Query => FromRow ( Expr Bool ) Bool where rowParser _ = fieldWith ( decode typeInformation )
instance m ~ Query => FromRow ( Expr Int32 ) Int32 where rowParser _ = fieldWith ( decode typeInformation )
instance m ~ Query => FromRow ( Expr Int64 ) Int64 where rowParser _ = fieldWith ( decode typeInformation )
instance m ~ Query => FromRow ( Expr String ) String where rowParser _ = fieldWith ( decode typeInformation )
instance m ~ Query => FromRow ( Expr Text ) Text where rowParser _ = fieldWith ( decode typeInformation )

instance m ~ Query => FromRow ( Expr ( Maybe Bool ) ) ( Maybe Bool ) where rowParser _ = fieldWith ( decode typeInformation )
instance m ~ Query => FromRow ( Expr ( Maybe Int32 ) ) ( Maybe Int32 ) where rowParser _ = fieldWith ( decode typeInformation )
instance m ~ Query => FromRow ( Expr ( Maybe Int64 ) ) ( Maybe Int64 ) where rowParser _ = fieldWith ( decode typeInformation )
instance m ~ Query => FromRow ( Expr ( Maybe String ) ) ( Maybe String ) where rowParser _ = fieldWith ( decode typeInformation )
instance m ~ Query => FromRow ( Expr ( Maybe Text ) ) ( Maybe Text ) where rowParser _ = fieldWith ( decode typeInformation )


instance (FromRow a1 b1, FromRow a2 b2) => FromRow (a1, a2) (b1, b2) where
  rowParser (a, b) =
    liftA2 (,) (rowParser a) (rowParser b)


instance
  ( f ~ Expr
  , g ~ Identity
  , HConstrainTable t Expr Unconstrained
  , HConstrainTable t Expr DBType
  , HConstrainTable t Identity DBType
  , HigherKindedTable t
  , Table ( MaybeTable ( t f ) )
  , Table ( t g )
  ) => FromRow ( MaybeTable ( t f ) ) ( Maybe ( t g ) ) where

  rowParser ( MaybeTable _ t ) = do
    rowExists <- fieldWith ( decode typeInformation )

    case rowExists of
      Just True ->
        Just <$> rowParser t

      _ ->
        Nothing <$ traverseTableC @DBType @RowParser @_ @(t Expr) nullField t


instance (HConstrainTable (Structure b1) (Context b1) Unconstrained, ExprTable b1, HConstrainTable (Structure b2) (Context b2) Unconstrained, ExprTable b2, FromRow a1 b1, FromRow a2 b2, Table a2, Table b1, Structure a1 ~ Structure b1, Structure a2 ~ Structure b2) => FromRow (MaybeTable (a1, a2)) (Maybe (b1, b2)) where
  rowParser ( MaybeTable _ ( x, y ) ) = do
    rowExists <- fieldWith ( decode typeInformation )

    case rowExists of
      Just True ->
        Just <$> liftA2 (,) (rowParser x) (rowParser y)

      _ ->
        Nothing
          <$ traverseTable @b1 nullField x
          <* traverseTable @b2 nullField y


nullField :: forall x f. C f x -> RowParser ( C f x )
nullField x = x <$ fieldWith (\_ _ -> pure ())
