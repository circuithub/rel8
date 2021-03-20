{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

{-# options_ghc -fno-warn-redundant-constraints #-}

module Rel8.Expr.Serialize
  ( litExpr
  , slitExpr
  , sparseValue
  )
where

-- base
import Data.Type.Equality ( (:~:)( Refl ) )
import Prelude hiding ( null )

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Expr.Opaleye ( scastExpr )
import Rel8.Kind.Blueprint
  ( SBlueprint
  , FromDBType, ToDBType
  , FromType, ToType
  , stoDBType, sfromDBType
  , blueprintRoundtripsViaDBType
  )
import Rel8.Kind.Nullability
  ( SNullability( SNullable, SNonNullable )
  , KnownNullability
  )
import Rel8.Schema.Value
  ( Value( NullableValue, NonNullableValue )
  , FromValue, ToValue
  , toValue
  )
import Rel8.Type ( DBType, blueprintForDBType, typeInformationFromBlueprint )
import Rel8.Type.Information ( TypeInformation(..) )


litExpr :: forall value nullability dbType a.
  ( KnownNullability nullability
  , '(nullability, value) ~ ToValue a
  , a ~ FromValue nullability value
  , value ~ ToType (FromDBType dbType)
  , dbType ~ ToDBType (FromType value)
  , DBType dbType
  )
  => a -> Expr nullability dbType
litExpr a = case blueprintRoundtripsViaDBType @dbType blueprint of
  Refl -> slitExpr blueprint (toValue a)
  where
    blueprint = blueprintForDBType @dbType


slitExpr :: ()
  => SBlueprint blueprint
  -> Value nullability (ToType blueprint)
  -> Expr nullability (ToDBType blueprint)
slitExpr blueprint = scastExpr info . Expr . \case
  NullableValue ma -> maybe null (encode . stoDBType blueprint) ma
  NonNullableValue a -> encode $ stoDBType blueprint a
  where
    info = typeInformationFromBlueprint blueprint
    TypeInformation {encode} = info
    null = Opaleye.ConstExpr Opaleye.NullLit


sparseValue :: ()
  => SNullability nullability
  -> SBlueprint blueprint
  -> Hasql.Row (Value nullability (ToType blueprint))
sparseValue nullability blueprint = case nullability of
  SNullable -> fmap NullableValue $ Hasql.column $ Hasql.nullable $
    sfromDBType blueprint <$> decode
  SNonNullable -> fmap NonNullableValue $ Hasql.column $ Hasql.nonNullable $
    sfromDBType blueprint <$> decode
  where
    TypeInformation {decode} = typeInformationFromBlueprint blueprint
