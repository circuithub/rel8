{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language StandaloneKindSignatures #-}
{-# language UndecidableInstances #-}

module Rel8.Type.Ord
  ( DBOrd
  , DBMax, DBMin
  )
where

-- base
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
import Rel8.Opaque ( Opaque )
import Rel8.Schema.Nullability ( Nullabilizes )
import Rel8.Type.Eq ( DBEq )

-- scientific
import Data.Scientific ( Scientific )

-- text
import Data.Text ( Text )
import qualified Data.Text.Lazy as Lazy ( Text )

-- time
import Data.Time.Calendar ( Day )
import Data.Time.Clock ( DiffTime, NominalDiffTime, UTCTime )
import Data.Time.LocalTime ( TimeOfDay, LocalTime )

-- uuid
import Data.UUID ( UUID )


type DBOrd :: Type -> Constraint
class DBEq a => DBOrd a
instance DBOrd Bool
instance DBOrd Char
instance DBOrd Int16
instance DBOrd Int32
instance DBOrd Int64
instance DBOrd Float
instance DBOrd Double
instance DBOrd Scientific
instance DBOrd UTCTime
instance DBOrd Day
instance DBOrd LocalTime
instance DBOrd TimeOfDay
instance DBOrd DiffTime
instance DBOrd NominalDiffTime
instance DBOrd Text
instance DBOrd Lazy.Text
instance DBOrd (CI Text)
instance DBOrd (CI Lazy.Text)
instance DBOrd ByteString
instance DBOrd Lazy.ByteString
instance DBOrd UUID
instance (DBOrd db, Nullabilizes db a) => DBOrd [a]
instance (DBOrd db, Nullabilizes db a) => DBOrd (NonEmpty a)
instance {-# OVERLAPPING #-} DBOrd Opaque


type DBMax :: Type -> Constraint
class DBOrd a => DBMax a
instance DBMax Bool
instance DBMax Char
instance DBMax Int16
instance DBMax Int32
instance DBMax Int64
instance DBMax Float
instance DBMax Double
instance DBMax Scientific
instance DBMax UTCTime
instance DBMax Day
instance DBMax LocalTime
instance DBMax TimeOfDay
instance DBMax DiffTime
instance DBMax NominalDiffTime
instance DBMax Text
instance DBMax Lazy.Text
instance DBMax (CI Text)
instance DBMax (CI Lazy.Text)
instance DBMax ByteString
instance DBMax Lazy.ByteString
instance (DBMax db, Nullabilizes db a) => DBMax [a]
instance (DBMax db, Nullabilizes db a) => DBMax (NonEmpty a)


type DBMin :: Type -> Constraint
class DBOrd a => DBMin a
instance DBMin Bool
instance DBMin Char
instance DBMin Int16
instance DBMin Int32
instance DBMin Int64
instance DBMin Float
instance DBMin Double
instance DBMin Scientific
instance DBMin UTCTime
instance DBMin Day
instance DBMin LocalTime
instance DBMin TimeOfDay
instance DBMin DiffTime
instance DBMin NominalDiffTime
instance DBMin Text
instance DBMin Lazy.Text
instance DBMin (CI Text)
instance DBMin (CI Lazy.Text)
instance DBMin ByteString
instance DBMin Lazy.ByteString
instance (DBMin db, Nullabilizes db a) => DBMin [a]
instance (DBMin db, Nullabilizes db a) => DBMin (NonEmpty a)
