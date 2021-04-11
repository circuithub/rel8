{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language UndecidableInstances #-}

module Rel8.Type.String
  ( DBString
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude ()

-- bytestring
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as Lazy ( ByteString )

-- case-insensitive
import Data.CaseInsensitive ( CI )

-- rel8
import Rel8.Type ( DBType )

-- text
import Data.Text ( Text )
import qualified Data.Text.Lazy as Lazy ( Text )


-- | The class of data types that support the @string_agg()@ aggregation
-- function.
type DBString :: Type -> Constraint
class DBType a => DBString a
instance DBString Text
instance DBString Lazy.Text
instance DBString (CI Text)
instance DBString (CI Lazy.Text)
instance DBString ByteString
instance DBString Lazy.ByteString
