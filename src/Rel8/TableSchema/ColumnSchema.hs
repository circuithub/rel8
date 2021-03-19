{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.TableSchema.ColumnSchema ( ColumnSchema(..) ) where

-- base
import Data.Kind ( Type )
import Data.String ( IsString( fromString ) )

-- rel8
import Rel8.HTable.Identity ( HIdentity( HIdentity, unHIdentity ) )
import Rel8.Info ( HasInfo )
import Rel8.Table ( Table( Columns, fromColumns, toColumns ) )


-- | The schema for a column in a table. To construct values of this type,
-- enable the @OverloadedStrings@ language extension and write literal Haskell
-- strings:
--
-- >>> :{
-- -- You would usually just inline this in your TableSchema definition.
-- authorColumns :: Author ColumnSchema
-- authorColumns = Author
--   { authorName = "name" 
--   , authorId = "author_id" 
--   , authorUrl = "url" 
--   }
-- :}
--
-- If you want to programatically create @ColumnSchema@'s, you can use 'Data.String.fromString':
--
-- >>> fromString ("hello" ++ "_" ++ "world") :: ColumnSchema Bool
newtype ColumnSchema (a :: Type) =
  ColumnSchema { columnName :: String }


-- | You can construct @ColumnSchema@ values by using @\{\-\# LANGUAGE
-- OverloadedStrings #-\}@ and writing literal strings in your source code.
instance IsString (ColumnSchema a) where
  fromString = ColumnSchema


instance (HasInfo a, f ~ ColumnSchema) => Table f (ColumnSchema a) where
  type Columns (ColumnSchema a) = HIdentity a
  toColumns = HIdentity
  fromColumns = unHIdentity
