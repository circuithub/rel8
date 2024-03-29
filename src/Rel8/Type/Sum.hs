{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language StandaloneKindSignatures #-}
{-# language UndecidableInstances #-}

module Rel8.Type.Sum
  ( DBSum
  )
where

-- base
import Data.Fixed (Fixed)
import Data.Int ( Int16, Int32, Int64 )
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Type ( DBType )
import Rel8.Type.Decimal (PowerOf10)

-- scientific
import Data.Scientific ( Scientific )

-- time
import Data.Time.LocalTime ( CalendarDiffTime )


-- | The class of database types that support the @sum()@ aggregation function.
type DBSum :: Type -> Constraint
class DBType a => DBSum a
instance DBSum Int16
instance DBSum Int32
instance DBSum Int64
instance PowerOf10 n => DBSum (Fixed n)
instance DBSum Float
instance DBSum Double
instance DBSum Scientific
instance DBSum CalendarDiffTime
