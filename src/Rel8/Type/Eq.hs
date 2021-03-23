{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language StandaloneKindSignatures #-}
{-# language UndecidableInstances #-}

module Rel8.Type.Eq
  ( DBEq
  )
where

-- aeson
import Data.Aeson ( Value )

-- base
import Data.List.NonEmpty ( NonEmpty )
import Data.Int ( Int16, Int32, Int64 )
import Data.Kind ( Constraint, Type )
import Prelude

-- bytestring
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as Lazy ( ByteString )

-- case-insensitive
import Data.CaseInsensitive ( CI )

-- rel8
import Rel8.Opaque ( Opaque )
import Rel8.Schema.Nullability ( Nullabilizes )
import Rel8.Type ( DBType )

-- scientific
import Data.Scientific ( Scientific )

-- text
import Data.Text ( Text )
import qualified Data.Text.Lazy as Lazy ( Text )

-- time
import Data.Time.Calendar ( Day )
import Data.Time.Clock ( UTCTime, DiffTime, NominalDiffTime )
import Data.Time.LocalTime ( TimeOfDay, LocalTime )

-- uuid
import Data.UUID ( UUID )


type DBEq :: Type -> Constraint
class DBType a => DBEq a


instance DBEq Bool
instance DBEq Char
instance DBEq Int16
instance DBEq Int32
instance DBEq Int64
instance DBEq Float
instance DBEq Double
instance DBEq Scientific
instance DBEq UTCTime
instance DBEq Day
instance DBEq LocalTime
instance DBEq TimeOfDay
instance DBEq DiffTime
instance DBEq NominalDiffTime
instance DBEq Text
instance DBEq Lazy.Text
instance DBEq (CI Text)
instance DBEq (CI Lazy.Text)
instance DBEq ByteString
instance DBEq Lazy.ByteString
instance DBEq UUID
instance DBEq Value
instance (DBEq db, Nullabilizes db a) => DBEq [a]
instance (DBEq db, Nullabilizes db a) => DBEq (NonEmpty a)
instance DBEq Opaque
