{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language GADTs #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Table.Selects ( Selects ) where

-- rel8
import Rel8.Expr ( Expr )
import Rel8.Table ( Table )
import Rel8.Table.Congruent ( Congruent )
import Rel8.TableSchema.ColumnSchema ( ColumnSchema )


-- | We say that @Table a@ "selects" @Table b@ if @a@ and @b@ are 'Congruent',
-- @a@ contains 'ColumnSchema's and @b@ contains 'Expr's.
class (Congruent schema exprs, Table Expr exprs, Table ColumnSchema schema) => Selects schema exprs | schema -> exprs
