{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Generic.Table.ADT
  ( GTableADT, GColumnsADT, gfromColumnsADT, gtoColumnsADT, gtableADT
  , GTableADT', GColumnsADT'
  , GToExprsADT, gfromResultADT, gtoResultADT
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
import Rel8.Generic.Table.Record
  ( GTable, GColumns, gtable
  , GToExprs, gfromResult, gtoResult
  )
import Rel8.Schema.Context.Label ( HLabelable, hlabeler, labeler, unlabeler )
import Rel8.Schema.HTable ( HTable, hmap )
import Rel8.Schema.HTable.Identity ( HIdentity( HType ), HType )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Nullify ( HNullify, hnulls, hnullify, hunnullify )
import Rel8.Schema.HTable.Product ( HProduct( HProduct ) )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Null ( Nullify )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec )
import Rel8.Schema.Result
  ( Col( R ), Result
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
  -> K.HContext -> (Type -> Type) -> Constraint
class GTableADT _Table _Columns context rep where
  gfromColumnsADT :: ()
    => (forall spec. context spec -> Col Result spec)
    -> (forall spec. Col Result spec -> context spec)
    -> (forall a. Eval (_Table a) => Eval (_Columns a) context -> a)
    -> GColumnsADT _Columns rep context
    -> rep x

  gtoColumnsADT :: ()
    => (forall spec. context spec -> Col Result spec)
    -> (forall spec. Col Result spec -> context spec)
    -> (forall a. Eval (_Table a) => a -> Eval (_Columns a) context)
    -> rep x
    -> GColumnsADT _Columns rep context

  gtableADT :: ()
    => (forall a proxy. Eval (_Table a) => proxy a -> Eval (_Columns a) context)
    -> (forall a labels. ()
        => SSpec ('Spec labels a)
        -> context ('Spec labels a)
        -> context ('Spec labels (Nullify a)))
    -> GColumnsADT _Columns rep context


instance
  ( htable ~ HLabel "tag" (HType Tag)
  , HTable (GColumnsADT _Columns (M1 D meta rep))
  , Eval (_Table (htable context))
  , Eval (_Columns (htable context)) ~ htable
  , GTableADT' _Table _Columns htable context rep
  , GToExprsADT' (Const _Table) _Columns htable rep rep
  )
  => GTableADT _Table _Columns context (M1 D meta rep)
 where
  gfromColumnsADT fromContext toContext fromColumns =
    gfromResultADT @(Const _Table) @_Columns @(M1 D meta rep) @(M1 D meta rep)
      (const (fromColumns . hmap toContext)) .
    hmap fromContext

  gtoColumnsADT fromContext toContext toColumns =
    hmap toContext .
    gtoResultADT @(Const _Table) @_Columns @(M1 D meta rep) @(M1 D meta rep)
      (const (hmap fromContext . toColumns))

  gtableADT table hnullifier =
    gtableADT' @_Table @_Columns @htable @context @rep table hnullifier htable
    where
      htable = table (Proxy @(htable context))


type GTableADT'
  :: (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> K.HTable -> K.HContext -> (Type -> Type) -> Constraint
class GTableADT' _Table _Columns htable context rep where
  gtableADT' :: ()
    => (forall a proxy. Eval (_Table a) => proxy a -> Eval (_Columns a) context)
    -> (forall a labels. ()
        => SSpec ('Spec labels a)
        -> context ('Spec labels a)
        -> context ('Spec labels (Nullify a)))
    -> htable context
    -> GColumnsADT' _Columns htable rep context


instance
  ( htable' ~ GColumnsADT' _Columns htable a
  , GTableADT' _Table _Columns htable context a
  , GTableADT' _Table _Columns htable' context b
  )
  => GTableADT' _Table _Columns htable context (a :+: b)
 where
  gtableADT' table hnullifier =
    gtableADT' @_Table @_Columns @_ @_ @b table hnullifier .
    gtableADT' @_Table @_Columns @_ @_ @a table hnullifier


instance meta ~ 'MetaCons label _fixity _isRecord =>
  GTableADT' _Table _Columns htable context (M1 C meta U1)
 where
  gtableADT' _ _ = id


instance {-# OVERLAPPABLE #-}
  ( HTable (GColumns _Columns rep)
  , GTable _Table _Columns context rep
  , HLabelable context
  , meta ~ 'MetaCons label _fixity _isRecord
  , KnownSymbol label
  , GColumnsADT' _Columns htable (M1 C ('MetaCons label _fixity _isRecord) rep) ~
      HProduct htable (HLabel label (HNullify (GColumns _Columns rep)))
  )
  => GTableADT' _Table _Columns htable context (M1 C meta rep)
 where
  gtableADT' table hnullifier htable =
    HProduct htable $
      hlabel hlabeler $
      hnullify hnullifier $
      gtable @_Table @_Columns @_ @rep table


type GToExprsADT
  :: (Type -> Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Type) -> (Type -> Type) -> Constraint
class GToExprsADT _ToExprs _Columns exprs rep where
  gfromResultADT :: ()
    => (forall expr a proxy.
        ( Eval (_ToExprs expr a)
        , HTable (Eval (_Columns expr))
        )
        => proxy expr
        -> Eval (_Columns expr) (Col Result)
        -> a)
    -> GColumnsADT _Columns exprs (Col Result)
    -> rep x

  gtoResultADT :: ()
    => (forall expr a proxy.
        ( Eval (_ToExprs expr a)
        , HTable (Eval (_Columns expr))
        )
        => proxy expr
        -> a
        -> Eval (_Columns expr) (Col Result))
    -> rep x
    -> GColumnsADT _Columns exprs (Col Result)


instance
  ( htable ~ HLabel "tag" (HType Tag)
  , GToExprsADT' _ToExprs _Columns htable exprs rep
  )
  => GToExprsADT _ToExprs _Columns (M1 D meta exprs) (M1 D meta rep)
 where
  gfromResultADT fromResult columns =
    case gfromResultADT' @_ToExprs @_Columns @_ @exprs fromResult tag columns of
      Just rep -> M1 rep
      _ -> error "ADT.fromColumns: mismatch between tag and data"
    where
      tag = (\(HType (R a)) -> a) . hunlabel @_ @"tag" unlabeler

  gtoResultADT toResult (M1 rep) =
    gtoResultADT' @_ToExprs @_Columns @_ @exprs toResult tag (Just rep)
    where
      tag = hlabel @_ @"tag" labeler . HType . R


type GToExprsADT'
  :: (Type -> Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> K.HTable -> (Type -> Type) -> (Type -> Type) -> Constraint
class GToExprsADT' _ToExprs _Columns htable exprs rep where
  gfromResultADT' :: ()
    => (forall expr a proxy.
        ( Eval (_ToExprs expr a)
        , HTable (Eval (_Columns expr))
        )
        => proxy expr
        -> Eval (_Columns expr) (Col Result)
        -> a)
    -> (htable (Col Result) -> Tag)
    -> GColumnsADT' _Columns htable exprs (Col Result)
    -> Maybe (rep x)

  gtoResultADT' :: ()
    => (forall expr a proxy.
        ( Eval (_ToExprs expr a)
        , HTable (Eval (_Columns expr))
        )
        => proxy expr
        -> a
        -> Eval (_Columns expr) (Col Result))
    -> (Tag -> htable (Col Result))
    -> Maybe (rep x)
    -> GColumnsADT' _Columns htable exprs (Col Result)

  extract :: GColumnsADT' _Columns htable exprs context -> htable context


instance
  ( htable' ~ GColumnsADT' _Columns htable a
  , GToExprsADT' _ToExprs _Columns htable a rep1
  , GToExprsADT' _ToExprs _Columns htable' b rep2
  )
  => GToExprsADT' _ToExprs _Columns htable (a :+: b) (rep1 :+: rep2)
 where
  gfromResultADT' fromResult f columns =
    case ma of
      Just a -> Just (L1 a)
      Nothing -> R1 <$>
        gfromResultADT' @_ToExprs @_Columns @_ @b @rep2
          fromResult
          (f . extract @_ToExprs @_Columns @_ @a @rep1)
          columns
    where
      ma =
        gfromResultADT' @_ToExprs @_Columns @_ @a @rep1
          fromResult
          f
          (extract @_ToExprs @_Columns @_ @b @rep2 columns)

  gtoResultADT' toResult tag = \case
    Just (L1 a) ->
      gtoResultADT' @_ToExprs @_Columns @_ @b @rep2
        toResult
        (\_ -> gtoResultADT' @_ToExprs @_Columns @_ @a @rep1
          toResult
          tag
          (Just a))
        Nothing
    Just (R1 b) ->
      gtoResultADT' @_ToExprs @_Columns @_ @b @rep2
        toResult
        (\tag' ->
          gtoResultADT' @_ToExprs @_Columns @_ @a @rep1
            toResult
            (\_ -> tag tag')
            Nothing)
        (Just b)
    Nothing ->
      gtoResultADT' @_ToExprs @_Columns @_ @b @rep2
        toResult
        (\_ -> gtoResultADT' @_ToExprs @_Columns @_ @a @rep1 toResult tag Nothing)
        Nothing

  extract =
    extract @_ToExprs @_Columns @_ @a @rep1 .
    extract @_ToExprs @_Columns @_ @b @rep2


instance
  ( meta ~ 'MetaCons label _fixity _isRecord
  , KnownSymbol label
  )
  => GToExprsADT' _ToExprs _Columns htable (M1 C meta U1) (M1 C meta U1)
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
  ( HTable (GColumns _Columns exprs)
  , GToExprs _ToExprs _Columns exprs rep
  , meta ~ 'MetaCons label _fixity _isRecord
  , KnownSymbol label
  , GColumnsADT' _Columns htable (M1 C meta exprs) ~
      HProduct htable (HLabel label (HNullify (GColumns _Columns exprs)))
  )
  => GToExprsADT' _ToExprs _Columns htable (M1 C meta exprs) (M1 C meta rep)
 where
  gfromResultADT' fromResult tag (HProduct a b)
    | tag a == tag' =
        M1 . gfromResult @_ToExprs @_Columns @exprs fromResult <$>
          hunnullify unnullifier (hunlabel unlabeler b)
    | otherwise = Nothing
    where
      tag' = Tag $ pack $ symbolVal (Proxy @label)

  gtoResultADT' toResult tag = \case
    Nothing -> HProduct (tag tag') (hlabel labeler (hnulls (const null)))
    Just (M1 rep) -> HProduct (tag tag') $
      hlabel labeler $
      hnullify nullifier $
      gtoResult @_ToExprs @_Columns @exprs toResult rep
    where
      tag' = Tag $ pack $ symbolVal (Proxy @label)

  extract (HProduct a _) = a


data Const :: (a -> Exp Constraint) -> a -> a -> Exp Constraint
type instance Eval (Const f x a) = (Eval (f a), x ~ a)
