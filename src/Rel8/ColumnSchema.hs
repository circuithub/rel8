{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language StandaloneKindSignatures #-}

module Rel8.ColumnSchema ( ColumnSchema(..) ) where

import Data.Kind ( Type )
import Data.String ( IsString(..) )
import Rel8.Type ( DBType )
import Rel8.Table ( Table(..) )
import Rel8.HTable.Identity ( HIdentity(..) )

type ColumnSchema :: Type -> Type
newtype ColumnSchema a =
  ColumnSchema { columnName :: String }


-- | You can construct @ColumnSchema@ values by using @\{\-\# LANGUAGE
-- OverloadedStrings #-\}@ and writing literal strings in your source code.
instance IsString (ColumnSchema a) where
  fromString = ColumnSchema


instance (DBType a, f ~ ColumnSchema) => Table f (ColumnSchema a) where
  type Columns (ColumnSchema a) = HIdentity a
  toColumns = HIdentity
  fromColumns = unHIdentity
