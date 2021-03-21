{-# language AllowAmbiguousTypes #-}
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

-- base
import Data.Kind ( Constraint, Type )
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude ()

-- rel8
import Rel8.Kind.Blueprint
  ( SBlueprint( SScalar, SVector )
  , KnownBlueprint, blueprintSing
  , FromDBType, ToDBType
  , blueprintRoundtripsViaDBType
  )
import Rel8.Opaque ( Opaque )
import Rel8.Type.Array ( arrayTypeInformation )
import Rel8.Type.Information ( TypeInformation(..) )


type DBType :: Type -> Constraint
class KnownBlueprint (FromDBType a) => DBType a
instance KnownBlueprint (FromDBType a) => DBType a
instance {-# OVERLAPPING #-} DBType Opaque


blueprintForDBType :: forall a. DBType a => SBlueprint (FromDBType a)
blueprintForDBType = blueprintSing @(FromDBType a)


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
