{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
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

module Rel8.Internal.Generic.Table.ADT
  ( GSerializeADT, GColumnsADT, gfromResultADT, gtoResultADT
  , GSerializeADT', GColumnsADT'
  , GADTTags
  )
where

-- base
import Data.Kind ( Constraint, Type )
import GHC.Generics
  ( Generic, Rep
  , (:+:)( L1, R1 ), M1( M1 ), U1( U1 )
  , C, D
  , Meta( MetaCons, MetaData )
  )
import GHC.TypeLits
  ( ErrorMessage( (:<>:), (:$$:), ShowType, Text ), TypeError
  , Nat, Symbol, type (+)
  )
import Prelude hiding ( null )

-- rel8
import Rel8.Internal.FCF ( Eval, Exp )
import Rel8.Internal.Generic.Table.ADT.Tag
  ( GTagColumns, gtagResult, gisTagResult
  )
import Rel8.Internal.Generic.Table.Record ( GSerialize, GColumns, gfromResult, gtoResult )
import Rel8.Internal.Schema.HTable ( HTable )
import Rel8.Internal.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Internal.Schema.HTable.Nullify ( HNullify, hnulls, hnullify, hunnullify )
import Rel8.Internal.Schema.HTable.Product ( HProduct( HProduct ) )
import qualified Rel8.Internal.Schema.Kind as K
import Rel8.Internal.Schema.Result ( Result, null, nullifier, unnullifier )
import Rel8.Internal.Type.Tag
  ( ADTTag( TagConstructor ), ADTTagFor
  , HasConstructor
  , Tag
  )


type GColumnsADT
  :: Type
  -> (Type -> Exp K.HTable)
  -> (Type -> Type) -> K.HTable
type family GColumnsADT tag _Columns rep where
  GColumnsADT tag _Columns (M1 D _ rep) =
    GColumnsADT' _Columns (GTagColumns tag) rep


type GColumnsADT'
  :: (Type -> Exp K.HTable)
  -> K.HTable -> (Type -> Type) -> K.HTable
type family GColumnsADT' _Columns htable rep  where
  GColumnsADT' _Columns htable (a :+: b) =
    GColumnsADT' _Columns (GColumnsADT' _Columns htable a) b
  GColumnsADT' _Columns htable (M1 C ('MetaCons _ _ _) U1) = htable
  GColumnsADT' _Columns htable (M1 C ('MetaCons label _ _) rep) =
    HProduct htable (HLabel label (HNullify (GColumns _Columns rep)))


type GADTTags :: Type -> (Type -> Type) -> Constraint
type family GADTTags tag rep where
  GADTTags Tag _rep = ()
  GADTTags tag (M1 D ('MetaData adt _module _package _newtype) rep) =
    ( Generic tag
    , GTagCount tag adt (Rep tag) rep
        (CountConstructors (Rep tag)) (CountConstructors rep)
    )


type GTagCount
  :: Type -> Symbol -> (Type -> Type) -> (Type -> Type) -> Nat -> Nat
  -> Constraint
type family GTagCount tag adt tagRep rep tagCount count where
  GTagCount tag adt (M1 D _meta tagRep) rep n n =
    ( GTagNullary tag tagRep
    , GTagMapping tag adt tagRep (GTagPairs tag rep '[])
    )
  GTagCount tag adt _tagRep _rep tagCount count = TypeError
    ( 'Text "The ADT tag type `" ' :<>: 'ShowType tag
        ' :<>: 'Text "` has " ' :<>: 'ShowType tagCount
        ' :<>: 'Text " constructors, but the ADT `" ' :<>: 'Text adt
        ' :<>: 'Text "` has " ' :<>: 'ShowType count ' :<>: 'Text "."
      ' :$$: 'Text "A tag type needs exactly one constructor per ADT constructor."
    )


type CountConstructors :: (Type -> Type) -> Nat
type family CountConstructors rep where
  CountConstructors (M1 D _meta rep) = CountConstructors rep
  CountConstructors (a :+: b) = CountConstructors a + CountConstructors b
  CountConstructors (M1 C _meta _rep) = 1
  CountConstructors _rep = 0


type GTagNullary :: Type -> (Type -> Type) -> Constraint
type family GTagNullary tag tagRep where
  GTagNullary tag (a :+: b) = (GTagNullary tag a, GTagNullary tag b)
  GTagNullary _tag (M1 C _meta U1) = ()
  GTagNullary tag (M1 C ('MetaCons tagName _fixity _isRecord) _rep) =
    TypeError
      ( 'Text "The ADT tag type `" ' :<>: 'ShowType tag
          ' :<>: 'Text "` has a constructor `" ' :<>: 'Text tagName
          ' :<>: 'Text "` with fields;"
        ' :$$: 'Text "the constructors of a tag type must all be nullary."
      )


type GTagPairs
  :: Type -> (Type -> Type) -> [(Symbol, Symbol)] -> [(Symbol, Symbol)]
type family GTagPairs tag rep acc where
  GTagPairs tag (a :+: b) acc = GTagPairs tag a (GTagPairs tag b acc)
  GTagPairs tag (M1 C ('MetaCons name _fixity _isRecord) _rep) acc =
    '(name, TagConstructor tag name) ': acc


type GTagMapping
  :: Type -> Symbol -> (Type -> Type) -> [(Symbol, Symbol)] -> Constraint
type family GTagMapping tag adt tagRep pairs where
  GTagMapping _tag _adt _tagRep '[] = ()
  GTagMapping tag adt tagRep ('(name, tagName) ': pairs) =
    ( GTagExists tag adt name tagName (HasConstructor tagName tagRep)
    , GTagUnique tag adt name tagName pairs
    , GTagMapping tag adt tagRep pairs
    )


type GTagExists :: Type -> Symbol -> Symbol -> Symbol -> Bool -> Constraint
type family GTagExists tag adt name tagName exists where
  GTagExists _tag _adt _name _tagName 'True = ()
  GTagExists tag adt name tagName 'False = TypeError
    ( 'Text "`TagConstructor` names `" ' :<>: 'Text tagName
        ' :<>: 'Text "` as the tag of the constructor `" ' :<>: 'Text name
        ' :<>: 'Text "` of `" ' :<>: 'Text adt ' :<>: 'Text "`,"
      ' :$$: 'Text "but the ADT tag type `" ' :<>: 'ShowType tag
        ' :<>: 'Text "` has no constructor `" ' :<>: 'Text tagName ' :<>: 'Text "`."
    )


type GTagUnique
  :: Type -> Symbol -> Symbol -> Symbol -> [(Symbol, Symbol)] -> Constraint
type family GTagUnique tag adt name tagName pairs where
  GTagUnique _tag _adt _name _tagName '[] = ()
  GTagUnique tag adt name tagName ('(name', tagName) ': _pairs) = TypeError
    ( 'Text "`TagConstructor` names `" ' :<>: 'Text tagName
        ' :<>: 'Text "` as the tag of both `" ' :<>: 'Text name
        ' :<>: 'Text "` and `" ' :<>: 'Text name' ' :<>: 'Text "` of `" ' :<>: 'Text adt
        ' :<>: 'Text "`;"
      ' :$$: 'Text "each constructor of an ADT needs its own constructor of the tag type `"
        ' :<>: 'ShowType tag ' :<>: 'Text "`."
    )
  GTagUnique tag adt name tagName (_pair ': pairs) =
    GTagUnique tag adt name tagName pairs


type GSerializeADT
  :: Type
  -> (Type -> Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Type) -> (Type -> Type) -> Constraint
class GSerializeADT tag _Serialize _Columns exprs rep where
  gfromResultADT :: ()
    => (forall expr a proxy. Eval (_Serialize expr a)
        => proxy expr -> Eval (_Columns expr) Result -> a)
    -> GColumnsADT tag _Columns exprs Result
    -> rep x

  gtoResultADT :: ()
    => (forall expr a proxy. Eval (_Serialize expr a)
        => proxy expr -> a -> Eval (_Columns expr) Result)
    -> rep x
    -> GColumnsADT tag _Columns exprs Result


instance
  ( GADTTags tag (M1 D meta exprs)
  , htable ~ GTagColumns tag
  , GSerializeADT' tag _Serialize _Columns htable exprs rep
  )
  => GSerializeADT tag _Serialize _Columns (M1 D meta exprs) (M1 D meta rep)
 where
  gfromResultADT fromResult columns =
    case gfromResultADT' @tag @_Serialize @_Columns @htable @exprs @rep fromResult id columns of
      Just rep -> M1 rep
      _ -> error "ADT.fromColumns: mismatch between tag and data"

  gtoResultADT toResult (M1 rep) =
    gtoResultADT' @tag @_Serialize @_Columns @htable @exprs @rep toResult id (Just rep)


type GSerializeADT'
  :: Type
  -> (Type -> Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> K.HTable -> (Type -> Type) -> (Type -> Type) -> Constraint
class GSerializeADT' tag _Serialize _Columns htable exprs rep where
  gfromResultADT' :: context ~ Result
    => (forall expr a proxy. Eval (_Serialize expr a)
        => proxy expr -> Eval (_Columns expr) context -> a)
    -> (htable Result -> GTagColumns tag Result)
    -> GColumnsADT' _Columns htable exprs context
    -> Maybe (rep x)

  gtoResultADT' :: context ~ Result
    => (forall expr a proxy. Eval (_Serialize expr a)
        => proxy expr -> a -> Eval (_Columns expr) context)
    -> (GTagColumns tag Result -> htable Result)
    -> Maybe (rep x)
    -> GColumnsADT' _Columns htable exprs context

  extract :: GColumnsADT' _Columns htable exprs context -> htable context


instance
  ( htable' ~ GColumnsADT' _Columns htable exprs1
  , GSerializeADT' tag _Serialize _Columns htable exprs1 a
  , GSerializeADT' tag _Serialize _Columns htable' exprs2 b
  )
  => GSerializeADT' tag _Serialize _Columns htable (exprs1 :+: exprs2) (a :+: b)
 where
  gfromResultADT' fromResult f columns =
    case ma of
      Just a -> Just (L1 a)
      Nothing -> R1 <$>
        gfromResultADT' @tag @_Serialize @_Columns @_ @exprs2 @b
          fromResult
          (f . extract @tag @_Serialize @_Columns @_ @exprs1 @a)
          columns
    where
      ma =
        gfromResultADT' @tag @_Serialize @_Columns @_ @exprs1 @a
          fromResult
          f
          (extract @tag @_Serialize @_Columns @_ @exprs2 @b columns)

  gtoResultADT' toResult tag = \case
    Just (L1 a) ->
      gtoResultADT' @tag @_Serialize @_Columns @_ @exprs2 @b
        toResult
        (\_ -> gtoResultADT' @tag @_Serialize @_Columns @_ @exprs1 @a
          toResult
          tag
          (Just a))
        Nothing
    Just (R1 b) ->
      gtoResultADT' @tag @_Serialize @_Columns @_ @exprs2 @b
        toResult
        (\tag' ->
          gtoResultADT' @tag @_Serialize @_Columns @_ @exprs1 @a
            toResult
            (\_ -> tag tag')
            Nothing)
        (Just b)
    Nothing ->
      gtoResultADT' @tag @_Serialize @_Columns @_ @exprs2 @b
        toResult
        (\_ -> gtoResultADT' @tag @_Serialize @_Columns @_ @exprs1 @a toResult tag Nothing)
        Nothing

  extract =
    extract @tag @_Serialize @_Columns @_ @exprs1 @a .
    extract @tag @_Serialize @_Columns @_ @exprs2 @b


instance
  ( meta ~ 'MetaCons label _fixity _isRecord
  , ADTTagFor tag label
  ) =>
  GSerializeADT' tag _Serialize _Columns _htable (M1 C meta U1) (M1 C meta U1)
 where
  gfromResultADT' _ tag columns
    | gisTagResult @tag @label (tag columns) = Just (M1 U1)
    | otherwise = Nothing

  gtoResultADT' _ tag _ = tag (gtagResult @tag @label)

  extract = id


instance {-# OVERLAPPABLE #-}
  ( HTable (GColumns _Columns exprs)
  , GSerialize _Serialize _Columns exprs rep
  , meta ~ 'MetaCons label _fixity _isRecord
  , ADTTagFor tag label
  , GColumnsADT' _Columns htable (M1 C ('MetaCons label _fixity _isRecord) exprs) ~
      HProduct htable (HLabel label (HNullify (GColumns _Columns exprs)))
  )
  => GSerializeADT' tag _Serialize _Columns htable (M1 C meta exprs) (M1 C meta rep)
 where
  gfromResultADT' fromResult tag (HProduct a b)
    | gisTagResult @tag @label (tag a) =
        M1 . gfromResult @_Serialize @_Columns @exprs @rep fromResult <$>
          hunnullify unnullifier (hunlabel b)
    | otherwise = Nothing

  gtoResultADT' toResult tag = \case
    Nothing -> HProduct (tag tag') (hlabel (hnulls (const null)))
    Just (M1 rep) -> HProduct (tag tag') $
      hlabel $
      hnullify nullifier $
      gtoResult @_Serialize @_Columns @exprs @rep toResult rep
    where
      tag' = gtagResult @tag @label

  extract (HProduct a _) = a
