{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Rel8.Generic.Table.ADT (
  GSerializeADT,
  GColumnsADT,
  gfromResultADT,
  gtoResultADT,
  GSerializeADT',
  GColumnsADT',
)
where

-- base
import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (
  C,
  D,
  M1 (M1),
  Meta (MetaCons),
  U1 (U1),
  (:+:) (L1, R1),
 )
import GHC.TypeLits (KnownSymbol, symbolVal)
import Prelude hiding (null)

-- rel8
import Rel8.FCF (Eval, Exp)
import Rel8.Generic.Table.Record (GColumns, GSerialize, gfromResult, gtoResult)
import Rel8.Schema.HTable (HTable)
import Rel8.Schema.HTable.Identity (HIdentity (HIdentity))
import Rel8.Schema.HTable.Label (HLabel, hlabel, hunlabel)
import Rel8.Schema.HTable.Nullify (HNullify, hnullify, hnulls, hunnullify)
import Rel8.Schema.HTable.Product (HProduct (HProduct))
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Result (Result, null, nullifier, unnullifier)
import Rel8.Type.Tag (Tag (Tag))

-- text
import Data.Text (pack)


type GColumnsADT ::
  (Type -> Exp K.HTable) ->
  (Type -> Type) ->
  K.HTable
type family GColumnsADT _Columns rep where
  GColumnsADT _Columns (M1 D _ rep) =
    GColumnsADT' _Columns (HLabel "tag" (HIdentity Tag)) rep


type GColumnsADT' ::
  (Type -> Exp K.HTable) ->
  K.HTable ->
  (Type -> Type) ->
  K.HTable
type family GColumnsADT' _Columns htable rep where
  GColumnsADT' _Columns htable (a :+: b) =
    GColumnsADT' _Columns (GColumnsADT' _Columns htable a) b
  GColumnsADT' _Columns htable (M1 C ('MetaCons _ _ _) U1) = htable
  GColumnsADT' _Columns htable (M1 C ('MetaCons label _ _) rep) =
    HProduct htable (HLabel label (HNullify (GColumns _Columns rep)))


type GSerializeADT ::
  (Type -> Type -> Exp Constraint) ->
  (Type -> Exp K.HTable) ->
  (Type -> Type) ->
  (Type -> Type) ->
  Constraint
class GSerializeADT _Serialize _Columns exprs rep where
  gfromResultADT ::
    () =>
    ( forall expr a proxy.
      Eval (_Serialize expr a) =>
      proxy expr ->
      Eval (_Columns expr) Result ->
      a
    ) ->
    GColumnsADT _Columns exprs Result ->
    rep x


  gtoResultADT ::
    () =>
    ( forall expr a proxy.
      Eval (_Serialize expr a) =>
      proxy expr ->
      a ->
      Eval (_Columns expr) Result
    ) ->
    rep x ->
    GColumnsADT _Columns exprs Result


instance
  ( htable ~ HLabel "tag" (HIdentity Tag)
  , GSerializeADT' _Serialize _Columns htable exprs rep
  ) =>
  GSerializeADT _Serialize _Columns (M1 D meta exprs) (M1 D meta rep)
  where
  gfromResultADT fromResult columns =
    case gfromResultADT' @_Serialize @_Columns @htable @exprs @rep fromResult tag columns of
      Just rep -> M1 rep
      _ -> error "ADT.fromColumns: mismatch between tag and data"
    where
      tag = (\(HIdentity (Identity a)) -> a) . hunlabel @"tag"


  gtoResultADT toResult (M1 rep) =
    gtoResultADT' @_Serialize @_Columns @htable @exprs @rep toResult tag (Just rep)
    where
      tag = hlabel @"tag" . HIdentity . Identity


type GSerializeADT' ::
  (Type -> Type -> Exp Constraint) ->
  (Type -> Exp K.HTable) ->
  K.HTable ->
  (Type -> Type) ->
  (Type -> Type) ->
  Constraint
class GSerializeADT' _Serialize _Columns htable exprs rep where
  gfromResultADT' ::
    context ~ Result =>
    ( forall expr a proxy.
      Eval (_Serialize expr a) =>
      proxy expr ->
      Eval (_Columns expr) context ->
      a
    ) ->
    (htable Result -> Tag) ->
    GColumnsADT' _Columns htable exprs context ->
    Maybe (rep x)


  gtoResultADT' ::
    context ~ Result =>
    ( forall expr a proxy.
      Eval (_Serialize expr a) =>
      proxy expr ->
      a ->
      Eval (_Columns expr) context
    ) ->
    (Tag -> htable Result) ->
    Maybe (rep x) ->
    GColumnsADT' _Columns htable exprs context


  extract :: GColumnsADT' _Columns htable exprs context -> htable context


instance
  ( htable' ~ GColumnsADT' _Columns htable exprs1
  , GSerializeADT' _Serialize _Columns htable exprs1 a
  , GSerializeADT' _Serialize _Columns htable' exprs2 b
  ) =>
  GSerializeADT' _Serialize _Columns htable (exprs1 :+: exprs2) (a :+: b)
  where
  gfromResultADT' fromResult f columns =
    case ma of
      Just a -> Just (L1 a)
      Nothing ->
        R1
          <$> gfromResultADT' @_Serialize @_Columns @_ @exprs2 @b
            fromResult
            (f . extract @_Serialize @_Columns @_ @exprs1 @a)
            columns
    where
      ma =
        gfromResultADT' @_Serialize @_Columns @_ @exprs1 @a
          fromResult
          f
          (extract @_Serialize @_Columns @_ @exprs2 @b columns)


  gtoResultADT' toResult tag = \case
    Just (L1 a) ->
      gtoResultADT' @_Serialize @_Columns @_ @exprs2 @b
        toResult
        ( \_ ->
            gtoResultADT' @_Serialize @_Columns @_ @exprs1 @a
              toResult
              tag
              (Just a)
        )
        Nothing
    Just (R1 b) ->
      gtoResultADT' @_Serialize @_Columns @_ @exprs2 @b
        toResult
        ( \tag' ->
            gtoResultADT' @_Serialize @_Columns @_ @exprs1 @a
              toResult
              (\_ -> tag tag')
              Nothing
        )
        (Just b)
    Nothing ->
      gtoResultADT' @_Serialize @_Columns @_ @exprs2 @b
        toResult
        (\_ -> gtoResultADT' @_Serialize @_Columns @_ @exprs1 @a toResult tag Nothing)
        Nothing


  extract =
    extract @_Serialize @_Columns @_ @exprs1 @a
      . extract @_Serialize @_Columns @_ @exprs2 @b


instance
  (meta ~ 'MetaCons label _fixity _isRecord, KnownSymbol label) =>
  GSerializeADT' _Serialize _Columns _htable (M1 C meta U1) (M1 C meta U1)
  where
  gfromResultADT' _ tag columns
    | tag columns == tag' = Just (M1 U1)
    | otherwise = Nothing
    where
      tag' = Tag $ pack $ symbolVal (Proxy @label)


  gtoResultADT' _ tag _ = tag tag'
    where
      tag' = Tag $ pack $ symbolVal (Proxy @label)


  extract = id


instance
  {-# OVERLAPPABLE #-}
  ( HTable (GColumns _Columns exprs)
  , GSerialize _Serialize _Columns exprs rep
  , meta ~ 'MetaCons label _fixity _isRecord
  , KnownSymbol label
  , GColumnsADT' _Columns htable (M1 C ('MetaCons label _fixity _isRecord) exprs)
      ~ HProduct htable (HLabel label (HNullify (GColumns _Columns exprs)))
  ) =>
  GSerializeADT' _Serialize _Columns htable (M1 C meta exprs) (M1 C meta rep)
  where
  gfromResultADT' fromResult tag (HProduct a b)
    | tag a == tag' =
        M1 . gfromResult @_Serialize @_Columns @exprs @rep fromResult
          <$> hunnullify unnullifier (hunlabel b)
    | otherwise = Nothing
    where
      tag' = Tag $ pack $ symbolVal (Proxy @label)


  gtoResultADT' toResult tag = \case
    Nothing -> HProduct (tag tag') (hlabel (hnulls (const null)))
    Just (M1 rep) ->
      HProduct (tag tag') $
        hlabel $
          hnullify nullifier $
            gtoResult @_Serialize @_Columns @exprs @rep toResult rep
    where
      tag' = Tag $ pack $ symbolVal (Proxy @label)


  extract (HProduct a _) = a
