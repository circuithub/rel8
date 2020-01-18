{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.FromRow where

import Data.Functor.Identity
import Data.Proxy
import Database.PostgreSQL.Simple.FromField ( FromField )
import Database.PostgreSQL.Simple.FromRow ( RowParser, field )
import Rel8.Column
import Rel8.Expr
import Rel8.HigherKinded
import Rel8.Query
import Rel8.Top
import Rel8.ZipLeaves


-- | @FromRow@ witnesses the one-to-one correspondence between the type @sql@,
-- which contains SQL expressions, and the type @haskell@, which contains the
-- Haskell decoding of rows containing @sql@ SQL expressions.
class FromRow sql haskell | sql -> haskell, haskell -> sql where
  rowParser :: sql -> RowParser haskell


instance ( FromRow sqlA haskellA, FromRow sqlB haskellB ) => FromRow ( sqlA, sqlB ) ( haskellA, haskellB ) where
  rowParser ( a, b ) =
    (,) <$> rowParser a <*> rowParser b


-- | Any higher-kinded records can be @SELECT@ed, as long as we know how to
-- decode all of the records constituent parts.
instance ( ZipRecord t ( Expr Query ) Identity Top, HigherKinded t, expr ~ Expr Query, identity ~ Identity, ZipRecord t ( Expr Query ) Identity FromField ) => FromRow ( t expr ) ( t identity ) where
  rowParser sql =
    zipLeaves
      ( Proxy @FromField )
      ( \_ _ -> C <$> field )
      sql
      sql


instance m ~ Query => FromRow ( Expr m Int ) Int where
  rowParser _ =
    field


instance m ~ Query => FromRow ( Expr m Bool ) Bool where
  rowParser _ =
    field
