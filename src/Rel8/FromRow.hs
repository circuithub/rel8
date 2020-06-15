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
import Database.PostgreSQL.Simple.FromRow ( RowParser, fieldWith )
import Rel8.Column
import Rel8.Expr
import Rel8.HigherKindedTable
import Rel8.Query
import Rel8.Table hiding ( field )
import Data.Text ( Text )


-- | @FromRow@ witnesses the one-to-one correspondence between the type @sql@,
-- which contains SQL expressions, and the type @haskell@, which contains the
-- Haskell decoding of rows containing @sql@ SQL expressions.
class ( Context sql ~ Expr, Table sql ) => FromRow sql haskell | sql -> haskell, haskell -> sql where
  rowParser :: sql -> RowParser haskell


-- | Any higher-kinded records can be @SELECT@ed, as long as we know how to
-- decode all of the records constituent part's.
instance ( HConstrainTable t Identity DBType, HConstrainTable t Identity Unconstrained, HigherKindedTable t, Table ( t expr ), expr ~ Expr, identity ~ Identity ) => FromRow ( t expr ) ( t identity ) where
  rowParser =
    traverseTableC @Select @DBType ( traverseCC @DBType \_ -> fieldWith ( decode typeInformation ) )


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
        Nothing <$ traverseTableC @Id @DBType nullField t


instance (FromRow a1 b1, FromRow a2 b2, Table a2, Table a2, Table b1, Table b2, Recontextualise a1 Id, Context (MapTable Id a1) ~ Expr, Recontextualise a2 Id, Context (MapTable Id a2) ~ Expr) => FromRow (MaybeTable (a1, a2)) (Maybe (b1, b2)) where
  rowParser ( MaybeTable _ ( x, y ) ) = do
    rowExists <- fieldWith ( decode typeInformation )

    case rowExists of
      Just True ->
        Just <$> liftA2 (,) (rowParser x) (rowParser y)

      _ ->
        Nothing
          <$ traverseTable @Id nullField x
          <* traverseTable @Id nullField y


nullField :: forall x f. C f x -> RowParser ( C f x )
nullField x = x <$ fieldWith (\_ _ -> pure ())
