{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language StandaloneKindSignatures #-}

module Rel8.Type.Sum
  ( DBSum
  )
where

-- base
import Data.Int ( Int16, Int32, Int64 )
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Kind.Bool ( IsList )
import Rel8.Type ( DBType )

-- scientific
import Data.Scientific ( Scientific )

-- time
import Data.Time.Clock ( DiffTime, NominalDiffTime )


type DBSum :: Type -> Constraint
class (DBType a, IsList a ~ 'False) => DBSum a
instance DBSum Int16
instance DBSum Int32
instance DBSum Int64
instance DBSum Float
instance DBSum Double
instance DBSum Scientific
instance DBSum DiffTime
instance DBSum NominalDiffTime
