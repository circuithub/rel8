{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MonoLocalBinds #-}
{-# language MultiParamTypeClasses #-}
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
import Rel8.Schema.Null ( Sql )
import Rel8.Type.Eq ( DBEq )

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


-- | The class of database types that support the @<@, @<=@, @>@ and @>=@
-- operators.
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
instance DBOrd CalendarDiffTime
instance DBOrd Text
instance DBOrd Lazy.Text
instance DBOrd (CI Text)
instance DBOrd (CI Lazy.Text)
instance DBOrd ByteString
instance DBOrd Lazy.ByteString
instance DBOrd UUID
instance Sql DBOrd a => DBOrd [a]
instance Sql DBOrd a => DBOrd (NonEmpty a)


-- | The class of database types that support the @max@ aggregation function.
type DBMax :: Type -> Constraint
class DBOrd a => DBMax a
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
instance DBMax CalendarDiffTime
instance DBMax Text
instance DBMax Lazy.Text
instance DBMax (CI Text)
instance DBMax (CI Lazy.Text)
instance DBMax ByteString
instance DBMax Lazy.ByteString
instance Sql DBMax a => DBMax [a]
instance Sql DBMax a => DBMax (NonEmpty a)


-- | The class of database types that support the @min@ aggregation function.
type DBMin :: Type -> Constraint
class DBOrd a => DBMin a
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
instance DBMin CalendarDiffTime
instance DBMin Text
instance DBMin Lazy.Text
instance DBMin (CI Text)
instance DBMin (CI Lazy.Text)
instance DBMin ByteString
instance DBMin Lazy.ByteString
instance Sql DBMin a => DBMin [a]
instance Sql DBMin a => DBMin (NonEmpty a)
