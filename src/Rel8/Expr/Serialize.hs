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
import Prelude hiding ( null )

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Kind.Blueprint
  ( SBlueprint
  , KnownBlueprint, blueprintSing
  , FromDBType, ToDBType
  , FromType, ToType
  , stoDBType, sfromDBType
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
import Rel8.Type ( DBType, TypeInformation(..), cast, typeInformation )


litExpr :: forall value nullability dbType a blueprint.
  ( KnownBlueprint blueprint
  , KnownNullability nullability
  , '(nullability, value) ~ ToValue a
  , a ~ FromValue nullability value
  , blueprint ~ FromType value
  , blueprint ~ FromDBType dbType
  , value ~ ToType blueprint
  , dbType ~ ToDBType blueprint
  , DBType dbType
  )
  => a -> Expr nullability dbType
litExpr a = slitExpr blueprint typeInformation (toValue a)
  where
    blueprint = blueprintSing @blueprint


slitExpr :: ()
  => SBlueprint blueprint
  -> TypeInformation (ToDBType blueprint)
  -> Value nullability (ToType blueprint)
  -> Expr nullability (ToDBType blueprint)
slitExpr blueprint info = Expr . cast info . \case
  NullableValue ma -> maybe null (encode . stoDBType blueprint) ma
  NonNullableValue a -> encode $ stoDBType blueprint a
  where
    TypeInformation {encode} = info
    null = Opaleye.ConstExpr Opaleye.NullLit


sparseValue :: ()
  => SNullability nullability
  -> SBlueprint blueprint
  -> TypeInformation (ToDBType blueprint)
  -> Hasql.Row (Value nullability (ToType blueprint))
sparseValue nullability blueprint info = case nullability of
  SNullable -> fmap NullableValue $ Hasql.column $ Hasql.nullable $
    sfromDBType blueprint <$> decode
  SNonNullable -> fmap NonNullableValue $ Hasql.column $ Hasql.nonNullable $
    sfromDBType blueprint <$> decode
  where
    TypeInformation {decode} = info
