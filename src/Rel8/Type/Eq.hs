{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MonoLocalBinds #-}
{-# language MultiParamTypeClasses #-}
{-# language StandaloneKindSignatures #-}
{-# language UndecidableInstances #-}

module Rel8.Type.Eq
  ( DBEq
  )
where

-- aeson
import Data.Aeson ( Value )

-- base
import Data.Fixed (Fixed)
import Data.Int ( Int16, Int32, Int64 )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import Prelude

-- bytestring
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as Lazy ( ByteString )

-- case-insensitive
import Data.CaseInsensitive ( CI )

-- rel8
import Rel8.Schema.Null ( Sql )
import Rel8.Type ( DBType )
import Rel8.Type.Decimal (PowerOf10)

-- scientific
import Data.Scientific ( Scientific )

-- text
import Data.Text ( Text )
import qualified Data.Text.Lazy as Lazy ( Text )

-- time
import Data.Time.Calendar ( Day )
import Data.Time.Clock ( UTCTime )
import Data.Time.LocalTime ( CalendarDiffTime, LocalTime, TimeOfDay )

-- uuid
import Data.UUID ( UUID )


-- | Database types that can be compared for equality in queries. If a type is
-- an instance of 'DBEq', it means we can compare expressions for equality
-- using the SQL @=@ operator.
type DBEq :: Type -> Constraint
class DBType a => DBEq a


instance DBEq Bool
instance DBEq Char
instance DBEq Int16
instance DBEq Int32
instance DBEq Int64
instance PowerOf10 n => DBEq (Fixed n)
instance DBEq Float
instance DBEq Double
instance DBEq Scientific
instance DBEq UTCTime
instance DBEq Day
instance DBEq LocalTime
instance DBEq TimeOfDay
instance DBEq CalendarDiffTime
instance DBEq Text
instance DBEq Lazy.Text
instance DBEq (CI Text)
instance DBEq (CI Lazy.Text)
instance DBEq ByteString
instance DBEq Lazy.ByteString
instance DBEq UUID
instance DBEq Value
instance Sql DBEq a => DBEq [a]
instance Sql DBEq a => DBEq (NonEmpty a)
