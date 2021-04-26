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
  ( GADTTable (fromGADTColumns, toGADTColumns)
  , GADTColumns
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
import Rel8.Generic.Table ( GTable, GColumns, fromGColumns, toGColumns )
import Rel8.Schema.Context ( Col( Result ) )
import Rel8.Schema.Context.Label ( labeler, unlabeler )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Nullify ( HNullify, hnulls, hnullify, hunnullify )
import Rel8.Schema.HTable.Product ( HProduct( HProduct ) )
import Rel8.Schema.HTable.Type ( HType( HType ) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Result ( Result )
import Rel8.Table ( null, nullifier, unnullifier )
import Rel8.Type.Tag ( Tag( Tag ) )

-- text
import Data.Text ( pack )


type GADTColumns
  :: (Type -> Exp K.HTable)
  -> (Type -> Type) -> K.HTable
type family GADTColumns _Columns rep where
  GADTColumns _Columns (M1 D _ rep) =
    GADTColumns' _Columns (HLabel "tag" (HType Tag)) rep


type GADTColumns'
  :: (Type -> Exp K.HTable)
  -> K.HTable -> (Type -> Type) -> K.HTable
type family GADTColumns' _Columns htable rep  where
  GADTColumns' _Columns htable (a :+: b) =
    GADTColumns' _Columns (GADTColumns' _Columns htable a) b
  GADTColumns' _Columns htable (M1 C ('MetaCons _ _ _) U1) = htable
  GADTColumns' _Columns htable (M1 C ('MetaCons label _ _) rep) =
    HProduct htable (HLabel label (HNullify (GColumns _Columns rep)))


type GADTTable
  :: (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Type) -> Constraint
class GADTTable _Table _Columns rep where
  fromGADTColumns :: ()
    => (forall a. Eval (_Table a) => Eval (_Columns a) (Col Result) -> a)
    -> GADTColumns _Columns rep (Col Result)
    -> rep x

  toGADTColumns :: ()
    => (forall a. Eval (_Table a) => a -> Eval (_Columns a) (Col Result))
    -> rep x
    -> GADTColumns _Columns rep (Col Result)


instance
  ( htable ~ HLabel "tag" (HType Tag)
  , GADTTable' _Table _Columns htable rep
  )
  => GADTTable _Table _Columns (M1 D meta rep)
 where
  fromGADTColumns fromColumns columns =
    case fromGADTColumns' @_Table @_Columns fromColumns tag columns of
      Just rep -> M1 rep
      _ -> error "ADT.fromColumns: mismatch between tag and data"
    where
      tag = (\(HType (Result a)) -> a) . hunlabel @_ @"tag" unlabeler

  toGADTColumns toColumns (M1 rep) =
    toGADTColumns' @_Table @_Columns toColumns tag (Just rep)
    where
      tag = hlabel @_ @"tag" labeler . HType . Result


type GADTTable'
  :: (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> K.HTable -> (Type -> Type) -> Constraint
class GADTTable' _Table _Columns htable rep where
  fromGADTColumns' :: ()
    => (forall a. Eval (_Table a) => Eval (_Columns a) (Col Result) -> a)
    -> (htable (Col Result) -> Tag)
    -> GADTColumns' _Columns htable rep (Col Result)
    -> Maybe (rep x)

  toGADTColumns' :: ()
    => (forall a. Eval (_Table a) => a -> Eval (_Columns a) (Col Result))
    -> (Tag -> htable (Col Result))
    -> Maybe (rep x)
    -> GADTColumns' _Columns htable rep (Col Result)

  extract :: GADTColumns' _Columns htable rep context -> htable context


instance
  ( htable' ~ GADTColumns' _Columns htable a
  , GADTTable' _Table _Columns htable a
  , GADTTable' _Table _Columns htable' b
  )
  => GADTTable' _Table _Columns htable (a :+: b)
 where
  fromGADTColumns' fromColumns f columns =
    case ma of
      Just a -> Just (L1 a)
      Nothing -> R1 <$>
        fromGADTColumns' @_Table @_Columns @_ @b
          fromColumns
          (f . extract @_Table @_Columns @_ @a)
          columns
    where
      ma =
        fromGADTColumns' @_Table @_Columns @_ @a
          fromColumns
          f
          (extract @_Table @_Columns @_ @b columns)

  toGADTColumns' toColumns tag = \case
    Just (L1 a) ->
      toGADTColumns' @_Table @_Columns @_ @b
        toColumns
        (\_ -> toGADTColumns' @_Table @_Columns @_ @a
          toColumns
          tag
          (Just a))
        Nothing
    Just (R1 b) ->
      toGADTColumns' @_Table @_Columns @_ @b
        toColumns
        (\tag' ->
          toGADTColumns' @_Table @_Columns @_ @a
            toColumns
            (\_ -> tag tag')
            Nothing)
        (Just b)
    Nothing ->
      toGADTColumns' @_Table @_Columns @_ @b
        toColumns
        (\_ -> toGADTColumns' @_Table @_Columns @_ @a toColumns tag Nothing)
        Nothing

  extract = extract @_Table @_Columns @_ @a . extract @_Table @_Columns @_ @b


instance KnownSymbol label =>
  GADTTable' _Table _Columns htable (M1 C ('MetaCons label _fixity _isRecord) U1)
 where
  fromGADTColumns' _ tag columns
    | tag columns == tag' = Just (M1 U1)
    | otherwise = Nothing
    where
      tag' = Tag $ pack $ symbolVal (Proxy @label)

  toGADTColumns' _ tag _ = tag tag'
    where
      tag' = Tag $ pack $ symbolVal (Proxy @label)

  extract = id


instance {-# OVERLAPPABLE #-}
  ( HTable (GColumns _Columns rep)
  , GTable _Table _Columns (Col Result) rep
  , KnownSymbol label
  , GADTColumns' _Columns htable (M1 C ('MetaCons label _fixity _isRecord) rep) ~
      HProduct htable (HLabel label (HNullify (GColumns _Columns rep)))
  )
  => GADTTable' _Table _Columns htable (M1 C ('MetaCons label _fixity _isRecord) rep)
 where
  fromGADTColumns' fromColumns tag (HProduct a b)
    | tag a == tag' =
        M1 . fromGColumns @_Table @_Columns fromColumns <$>
          hunnullify unnullifier (hunlabel unlabeler b)
    | otherwise = Nothing
    where
      tag' = Tag $ pack $ symbolVal (Proxy @label)

  toGADTColumns' toColumns tag = \case
    Nothing -> HProduct (tag tag') (hlabel labeler (hnulls null))
    Just (M1 rep) -> HProduct (tag tag') $
      hlabel labeler $
      hnullify nullifier $
      toGColumns @_Table @_Columns toColumns rep
    where
      tag' = Tag $ pack $ symbolVal (Proxy @label)

  extract (HProduct a _) = a
