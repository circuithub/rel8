{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
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
import Rel8.Schema.Nullability ( Unnullify, HasNullability, Sql )
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


-- | Database types that can be compared for equality in queries. If a type is
-- an instance of 'DBEq', it means we can compare expressions for equality
-- using the SQL @=@ operator.
-- 
-- [ @DBEq@ with @newtype@s ]
-- 
-- Like with 'Rel8.DBType', @DBEq@ plays well with generalized newtype
-- deriving.  The example given for @DBType@ added a @UserId@ @newtype@, but
-- without a @DBEq@ instance won't actually be able to use that in joins or
-- where-clauses, because it lacks equality. We can add this by changing our
-- @newtype@ definition to:
-- 
-- >>> newtype UserId = UserId { toInt32 :: Int32 } deriving newtype (DBType, DBEq)
-- 
-- This will re-use the equality logic for @Int32@, which is to just use the
-- @=@ operator.
-- 
-- [ @DBEq@ with @DeriveAnyType@ ]
-- 
-- You can also use @DBEq@ with the @DeriveAnyType@ extension to easily add
-- equality to your type, assuming that @=@ is sufficient on @DBType@ encoded
-- values. Extending the example from 'Rel8.ReadShow''s 'Rel8.DBType' instance,
-- we could add equality to @Color@ by writing:
-- 
-- >>> :{
-- data Color = Red | Green | Blue | Purple | Gold
--   deriving (Generic, Show, Read, DBEq)
--   deriving DBType via ReadShow Color
-- :}
-- 
-- This means @Color@s will be treated as the literal strings @"Red"@,
-- @"Green"@, etc, in the database, and they can be compared for equality by
-- just using @=@.
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
instance Sql DBEq a => DBEq [a]
instance Sql DBEq a => DBEq (NonEmpty a)
instance DBEq Opaque


instance {-# INCOHERENT #-} (HasNullability a, DBEq (Unnullify a)) =>
  Sql DBEq a
