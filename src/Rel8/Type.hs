{-# language AllowAmbiguousTypes #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Type
  ( DBType, typeInformation
  , blueprintForDBType, typeInformationFromBlueprint
  )
where

-- aeson
import Data.Aeson ( Value )

-- base
import Data.Int ( Int16, Int32, Int64 )
import Data.Kind ( Constraint, Type )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude

-- bytestring
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as Lazy ( ByteString )

-- case-insensitive
import Data.CaseInsensitive ( CI )

-- rel8
import Rel8.Kind.Blueprint
  ( SBlueprint( SScalar, SVector )
  , KnownBlueprint, blueprintSing
  , FromDBType, ToDBType
  , blueprintRoundtripsViaDBType
  )
import Rel8.Kind.Emptiability ( KnownEmptiability, emptiabilitySing )
import Rel8.Kind.Nullability ( KnownNullability, nullabilitySing )
import Rel8.Type.Array ( Array, arrayTypeInformation )
import Rel8.Type.Information ( TypeInformation(..) )

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


type DBType :: Type -> Constraint
class DBType a where
  blueprintForDBType :: SBlueprint (FromDBType a)

  default blueprintForDBType :: KnownBlueprint (FromDBType a)
    => SBlueprint (FromDBType a)
  blueprintForDBType = blueprintSing @(FromDBType a)


instance DBType Bool
instance DBType Char
instance DBType Int16
instance DBType Int32
instance DBType Int64
instance DBType Float
instance DBType Double
instance DBType Scientific
instance DBType UTCTime
instance DBType Day
instance DBType LocalTime
instance DBType TimeOfDay
instance DBType DiffTime
instance DBType NominalDiffTime
instance DBType Text
instance DBType Lazy.Text
instance DBType (CI Text)
instance DBType (CI Lazy.Text)
instance DBType ByteString
instance DBType Lazy.ByteString
instance DBType UUID
instance DBType Value
instance
  ( KnownEmptiability emptiability
  , KnownNullability nullability
  , DBType a
  ) => DBType (Array emptiability nullability a)
 where
  blueprintForDBType = SVector emptiability nullability blueprint
    where
      emptiability = emptiabilitySing @emptiability
      nullability = nullabilitySing @nullability
      blueprint = blueprintForDBType @a


typeInformation :: forall a. DBType a => TypeInformation a
typeInformation = case blueprintForDBType @a of
  blueprint -> case blueprintRoundtripsViaDBType @a blueprint of
    Refl -> typeInformationFromBlueprint blueprint


typeInformationFromBlueprint :: ()
  => SBlueprint blueprint -> TypeInformation (ToDBType blueprint)
typeInformationFromBlueprint = \case
  SScalar info -> info
  SVector emptiability nullability blueprint ->
    arrayTypeInformation emptiability nullability
      (typeInformationFromBlueprint blueprint)
