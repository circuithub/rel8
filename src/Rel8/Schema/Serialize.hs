{-# language FlexibleInstances #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Schema.Serialize
  ( Encodable( Encoding )
  , Constraints
  , Exprable
  )
where

-- aeson
import Data.Aeson ( Value )

-- base
import Data.Int ( Int16, Int32, Int64 )
import Data.List.NonEmpty ( NonEmpty )
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy )
import Prelude

-- bytestring
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as Lazy ( ByteString )

-- case-insensitive
import Data.CaseInsensitive ( CI )

-- rel8
import {-# SOURCE #-} Rel8.Expr ( Expr )

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


type Constraints :: Type -> Type -> Constraint
type family Constraints encoding a
type instance Constraints (Proxy Expr) _a = ()


type Encodable :: Type -> Constraint
class Constraints (Encoding a) a => Encodable a where
  type Encoding a :: Type
  type Encoding _a = Proxy (Expr :: Type -> Type)


instance Encodable Bool
instance Encodable Char
instance Encodable Int16
instance Encodable Int32
instance Encodable Int64
instance Encodable Float
instance Encodable Double
instance Encodable Scientific
instance Encodable UTCTime
instance Encodable Day
instance Encodable LocalTime
instance Encodable TimeOfDay
instance Encodable CalendarDiffTime
instance Encodable Text
instance Encodable Lazy.Text
instance Encodable (CI Text)
instance Encodable (CI Lazy.Text)
instance Encodable ByteString
instance Encodable Lazy.ByteString
instance Encodable UUID
instance Encodable Value
instance Encodable [a]
instance Encodable (NonEmpty a)
instance Encodable (Maybe a)


class (Encodable a, Encoding a ~ Proxy (Expr :: Type -> Type)) => Exprable a
instance (Encodable a, Encoding a ~ Proxy (Expr :: Type -> Type)) => Exprable a
