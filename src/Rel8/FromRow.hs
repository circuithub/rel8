{-# language BlockArguments #-}
{-# language DataKinds #-}
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
import Data.Kind ( Type )
import Data.Proxy
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

type family Choose (a :: Type) (b :: Type) :: Choice where
  Choose (a, b) (x, y) = 'TABLE
  Choose (Expr a) a = 'EXPR
  Choose (t Expr) (t Identity) = 'TABLE
  Choose (MaybeTable a) (Maybe b) = 'TABLE

data Choice = EXPR | TABLE

type family ExprType (a :: Type) :: Type where
  ExprType (a, b) = (ExprType a, ExprType b)
  ExprType (t Identity) = t Expr
  ExprType (Maybe (t Identity)) = MaybeTable (t Expr)
  ExprType (Maybe a) = Expr (Maybe a)
  ExprType a = Expr a

type family ResultType (a :: Type) :: Type where
  ResultType (a, b) = (ResultType a, ResultType b)
  ResultType (t Expr) = t Identity
  ResultType (Expr a) = a
  ResultType (MaybeTable a) = Maybe (ResultType a)

instance (ExprTable a, FromRowChoice (Choose a b) a b, a ~ ExprType b, b ~ ResultType a) => FromRow a b where
  rowParser = rowParser_ (Proxy @(Choose a b))

class ExprTable a => FromRowChoice (choice :: Choice) (a :: Type) (b :: Type) where
  rowParser_ :: proxy choice -> a -> RowParser b

-- | Any higher-kinded records can be @SELECT@ed, as long as we know how to
-- decode all of the records constituent part's.
instance (expr ~ Expr, identity ~ Identity, ExprTable (t expr), Table (t identity), HConstrainTable t Identity DBType) => FromRowChoice 'TABLE ( t expr ) ( t identity ) where
  rowParser_ _ =
    traverseTableC @DBType ( traverseCC @DBType \_ -> fieldWith ( decode typeInformation ) )


instance DBType a => FromRowChoice 'EXPR (Expr a) a where rowParser_ _ _ = fieldWith (decode typeInformation)


instance (FromRow a1 b1, FromRow a2 b2) => FromRowChoice 'TABLE (a1, a2) (b1, b2) where
  rowParser_ _ (a, b) =
    liftA2 (,) (rowParser a) (rowParser b)


instance
  ( Context a ~ Expr
  , Table a
  , HConstrainTable (Structure a) Expr Unconstrained
  , HConstrainTable (Structure a) Expr DBType
  , FromRow a b
  ) => FromRowChoice 'TABLE (MaybeTable a) (Maybe b) where

  rowParser_ _ ( MaybeTable _ t ) = do
    rowExists <- fieldWith ( decode typeInformation )

    case rowExists of
      Just True ->
        Just <$> rowParser t

      _ ->
        Nothing <$ traverseTableC @DBType @RowParser @_ @a nullField t


nullField :: forall x f. C f x -> RowParser ( C f x )
nullField x = x <$ fieldWith (\_ _ -> pure ())
