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
import Data.Aeson ( Value )
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.CaseInsensitive (CI)
import Data.Functor.Identity
import Data.Int
import Data.Scientific ( Scientific )
import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Lazy
import Data.Time
import Data.UUID
import Database.PostgreSQL.Simple.FromRow ( RowParser, fieldWith )
import Rel8.Column
import Rel8.Expr
import Rel8.MaybeTable
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


instance FromRow (Expr (CI Lazy.Text)) (CI Lazy.Text) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (CI Strict.Text)) (CI Strict.Text) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe (CI Lazy.Text))) (Maybe (CI Lazy.Text)) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe (CI Strict.Text))) (Maybe (CI Strict.Text)) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe Bool)) (Maybe Bool) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe Day)) (Maybe Day) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe Double)) (Maybe Double) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe Float)) (Maybe Float) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe Int32)) (Maybe Int32) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe Int64)) (Maybe Int64) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe Lazy.ByteString)) (Maybe Lazy.ByteString) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe Lazy.Text)) (Maybe Lazy.Text) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe LocalTime)) (Maybe LocalTime) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe Scientific)) (Maybe Scientific) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe Strict.ByteString)) (Maybe Strict.ByteString) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe Strict.Text)) (Maybe Strict.Text) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe String)) (Maybe String) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe TimeOfDay)) (Maybe TimeOfDay) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe UTCTime)) (Maybe UTCTime) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe UUID)) (Maybe UUID) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe Value)) (Maybe Value) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr (Maybe ZonedTime)) (Maybe ZonedTime) where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr Bool) Bool where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr Day) Day where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr Double) Double where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr Float) Float where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr Int32) Int32 where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr Int64) Int64 where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr Lazy.ByteString) Lazy.ByteString where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr Lazy.Text) Lazy.Text where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr LocalTime) LocalTime where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr Scientific) Scientific where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr Strict.ByteString) Strict.ByteString where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr Strict.Text) Strict.Text where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr String) String where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr TimeOfDay) TimeOfDay where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr UTCTime) UTCTime where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr UUID) UUID where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr Value) Value where rowParser _ = fieldWith (decode typeInformation)
instance FromRow (Expr ZonedTime) ZonedTime where rowParser _ = fieldWith (decode typeInformation)


instance (FromRow a1 b1, FromRow a2 b2) => FromRow (a1, a2) (b1, b2) where
  rowParser (a, b) =
    liftA2 (,) (rowParser a) (rowParser b)


instance (FromRow a1 b1, FromRow a2 b2, FromRow a3 b3) => FromRow (a1, a2, a3) (b1, b2, b3) where
  rowParser (a, b, c) =
    (,,) <$> rowParser a <*> rowParser b <*> rowParser c


instance (FromRow a1 b1, FromRow a2 b2, FromRow a3 b3, FromRow a4 b4) => FromRow (a1, a2, a3, a4) (b1, b2, b3, b4) where
  rowParser (a, b, c, d) =
    (,,,) <$> rowParser a <*> rowParser b <*> rowParser c <*> rowParser d


instance (FromRow a1 b1, FromRow a2 b2, FromRow a3 b3, FromRow a4 b4, FromRow a5 b5) => FromRow (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5) where
  rowParser (a, b, c, d, e) =
    (,,,,) <$> rowParser a <*> rowParser b <*> rowParser c <*> rowParser d <*> rowParser e


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
