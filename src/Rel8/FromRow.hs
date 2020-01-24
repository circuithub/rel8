{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.FromRow where

import Data.Functor.Identity
import Data.Int
import Database.PostgreSQL.Simple.FromField ( FromField )
import Database.PostgreSQL.Simple.FromRow ( RowParser, field )
import Rel8.Column
import Rel8.Expr
import Rel8.HigherKindedTable
import Rel8.Query
import Rel8.Table hiding ( field )
import Rel8.Unconstrained


-- | @FromRow@ witnesses the one-to-one correspondence between the type @sql@,
-- which contains SQL expressions, and the type @haskell@, which contains the
-- Haskell decoding of rows containing @sql@ SQL expressions.
class ( Context sql ~ Expr Query, Table sql ) => FromRow sql haskell | sql -> haskell, haskell -> sql where
  rowParser :: sql -> RowParser haskell


-- | Any higher-kinded records can be @SELECT@ed, as long as we know how to
-- decode all of the records constituent part's.
instance ( HConstrainTable t Identity FromField, HConstrainTable t Identity Unconstrained, HigherKindedTable t, Table ( t expr ), expr ~ Expr Query, identity ~ Identity ) => FromRow ( t expr ) ( t identity ) where
  rowParser =
    traverseTableC @Select @FromField ( traverseCC @FromField \_ -> field )


instance m ~ Query => FromRow ( Expr m Int32 ) Int32 where
  rowParser _ =
    field


instance m ~ Query => FromRow ( Expr m Bool ) Bool where
  rowParser _ =
    field
