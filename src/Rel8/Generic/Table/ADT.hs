{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Generic.Table.ADT
  ( GTableADT, GColumnsADT, gfromResultADT, gtoResultADT
  , GTableADT', GColumnsADT'
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Data.Proxy ( Proxy( Proxy ) )
import GHC.Generics
  ( (:+:)( L1, R1 ), M1( M1 ), U1( U1 )
  , C, D
  , Meta( MetaCons )
  )
import GHC.TypeLits ( KnownSymbol, symbolVal )
import Prelude hiding ( null )

-- rel8
import Rel8.FCF ( Eval, Exp )
import Rel8.Generic.Map ( GMap )
import Rel8.Generic.Table.Record ( GTable, GColumns, gfromResult, gtoResult )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Identity ( HIdentity( HType ), HType )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Nullify ( HNullify, hnulls, hnullify, hunnullify )
import Rel8.Schema.HTable.Product ( HProduct( HProduct ) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Result
  ( Result( R )
  , null, nullifier, unnullifier
  )
import Rel8.Type.Tag ( Tag( Tag ) )

-- text
import Data.Text ( pack )


type GColumnsADT
  :: (Type -> Exp K.HTable)
  -> (Type -> Type) -> K.HTable
type family GColumnsADT _Columns rep where
  GColumnsADT _Columns (M1 D _ rep) =
    GColumnsADT' _Columns (HLabel "tag" (HType Tag)) rep


type GColumnsADT'
  :: (Type -> Exp K.HTable)
  -> K.HTable -> (Type -> Type) -> K.HTable
type family GColumnsADT' _Columns htable rep  where
  GColumnsADT' _Columns htable (a :+: b) =
    GColumnsADT' _Columns (GColumnsADT' _Columns htable a) b
  GColumnsADT' _Columns htable (M1 C ('MetaCons _ _ _) U1) = htable
  GColumnsADT' _Columns htable (M1 C ('MetaCons label _ _) rep) =
    HProduct htable (HLabel label (HNullify (GColumns _Columns rep)))


type GTableADT
  :: (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Exp Type)
  -> (Type -> Type) -> Constraint
class GTableADT _Table _Columns _FromExprs rep where
  gfromResultADT :: ()
    => (forall a proxy. Eval (_Table a)
        => proxy a -> Eval (_Columns a) Result -> Eval (_FromExprs a))
    -> GColumnsADT _Columns rep Result
    -> GMap _FromExprs rep x

  gtoResultADT :: ()
    => (forall a proxy. Eval (_Table a)
        => proxy a -> Eval (_FromExprs a) -> Eval (_Columns a) Result)
    -> GMap _FromExprs rep x
    -> GColumnsADT _Columns rep Result


instance
  ( htable ~ HLabel "tag" (HType Tag)
  , GTableADT' _Table _Columns _FromExprs htable rep
  )
  => GTableADT _Table _Columns _FromExprs (M1 D meta rep)
 where
  gfromResultADT fromResult columns =
    case gfromResultADT' @_Table @_Columns @_FromExprs @htable @rep fromResult tag columns of
      Just rep -> M1 rep
      _ -> error "ADT.fromColumns: mismatch between tag and data"
    where
      tag = (\(HType (R a)) -> a) . hunlabel @"tag"

  gtoResultADT toResult (M1 rep) =
    gtoResultADT' @_Table @_Columns @_FromExprs @htable @rep toResult tag (Just rep)
    where
      tag = hlabel @"tag" . HType . R


type GTableADT'
  :: (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Exp Type)
  -> K.HTable -> (Type -> Type) -> Constraint
class GTableADT' _Table _Columns _FromExprs htable rep where
  gfromResultADT' :: context ~ Result
    => (forall a proxy. Eval (_Table a)
        => proxy a -> Eval (_Columns a) context -> Eval (_FromExprs a))
    -> (htable Result -> Tag)
    -> GColumnsADT' _Columns htable rep context
    -> Maybe (GMap _FromExprs rep x)

  gtoResultADT' :: context ~ Result
    => (forall a proxy. Eval (_Table a)
        => proxy a -> Eval (_FromExprs a) -> Eval (_Columns a) context)
    -> (Tag -> htable Result)
    -> Maybe (GMap _FromExprs rep x)
    -> GColumnsADT' _Columns htable rep context

  extract :: GColumnsADT' _Columns htable rep context -> htable context


instance
  ( htable' ~ GColumnsADT' _Columns htable a
  , GTableADT' _Table _Columns _FromExprs htable a
  , GTableADT' _Table _Columns _FromExprs htable' b
  )
  => GTableADT' _Table _Columns _FromExprs htable (a :+: b)
 where
  gfromResultADT' fromResult f columns =
    case ma of
      Just a -> Just (L1 a)
      Nothing -> R1 <$>
        gfromResultADT' @_Table @_Columns @_FromExprs @_ @b
          fromResult
          (f . extract @_Table @_Columns @_FromExprs @_ @a)
          columns
    where
      ma =
        gfromResultADT' @_Table @_Columns @_FromExprs @_ @a
          fromResult
          f
          (extract @_Table @_Columns @_FromExprs @_ @b columns)

  gtoResultADT' toResult tag = \case
    Just (L1 a) ->
      gtoResultADT' @_Table @_Columns @_FromExprs @_ @b
        toResult
        (\_ -> gtoResultADT' @_Table @_Columns @_FromExprs @_ @a
          toResult
          tag
          (Just a))
        Nothing
    Just (R1 b) ->
      gtoResultADT' @_Table @_Columns @_FromExprs @_ @b
        toResult
        (\tag' ->
          gtoResultADT' @_Table @_Columns @_FromExprs @_ @a
            toResult
            (\_ -> tag tag')
            Nothing)
        (Just b)
    Nothing ->
      gtoResultADT' @_Table @_Columns @_FromExprs @_ @b
        toResult
        (\_ -> gtoResultADT' @_Table @_Columns @_FromExprs @_ @a toResult tag Nothing)
        Nothing

  extract =
    extract @_Table @_Columns @_FromExprs @_ @a .
    extract @_Table @_Columns @_FromExprs @_ @b


instance (meta ~ 'MetaCons label _fixity _isRecord, KnownSymbol label) =>
  GTableADT' _Table _Columns _FromExprs htable (M1 C meta U1)
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


instance {-# OVERLAPPABLE #-}
  ( HTable (GColumns _Columns rep)
  , GTable _Table _Columns _FromExprs rep
  , meta ~ 'MetaCons label _fixity _isRecord
  , KnownSymbol label
  , GColumnsADT' _Columns htable (M1 C ('MetaCons label _fixity _isRecord) rep) ~
      HProduct htable (HLabel label (HNullify (GColumns _Columns rep)))
  )
  => GTableADT' _Table _Columns _FromExprs htable (M1 C meta rep)
 where
  gfromResultADT' fromResult tag (HProduct a b)
    | tag a == tag' =
        M1 . gfromResult @_Table @_Columns @_FromExprs @rep fromResult <$>
          hunnullify unnullifier (hunlabel b)
    | otherwise = Nothing
    where
      tag' = Tag $ pack $ symbolVal (Proxy @label)

  gtoResultADT' toResult tag = \case
    Nothing -> HProduct (tag tag') (hlabel (hnulls (const null)))
    Just (M1 rep) -> HProduct (tag tag') $
      hlabel $
      hnullify nullifier $
      gtoResult @_Table @_Columns @_FromExprs @rep toResult rep
    where
      tag' = Tag $ pack $ symbolVal (Proxy @label)

  extract (HProduct a _) = a
