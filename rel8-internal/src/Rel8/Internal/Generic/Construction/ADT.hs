{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Rel8.Internal.Generic.Construction.ADT
  ( GConstructableADT
  , GBuildADT, gbuildADT, gunbuildADT
  , GConstructADT, gconstructADT, gdeconstructADT
  , GFields, RepresentableFields, gftabulate, gfindex
  , GConstructors, RepresentableConstructors, gctabulate, gcindex
  , GConstructorADT, GMakeableADT, gmakeADT
  )
where

-- base
import Data.Functor.Identity ( runIdentity )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import GHC.Generics
  ( (:+:), (:*:)( (:*:) ), M1, U1
  , C, D
  , Meta( MetaData, MetaCons )
  )
import GHC.TypeLits
  ( ErrorMessage( (:<>:), Text ), TypeError
  , Symbol
  )
import Prelude hiding ( null )

-- rel8
import Rel8.Internal.Expr ( Expr )
import Rel8.Internal.FCF ( Exp )
import Rel8.Internal.Generic.Construction.Record
  ( GConstruct, GConstructable, gconstruct, gdeconstruct
  , GFields, Representable, gtabulate, gindex
  , FromColumns, ToColumns
  )
import Rel8.Internal.Generic.Table.ADT ( GColumnsADT, GColumnsADT' )
import Rel8.Internal.Generic.Table.ADT.Tag
  ( GTagColumns, gtagExpr, gisTagExpr
  )
import Rel8.Internal.Generic.Table.Record ( GColumns )
import Rel8.Internal.Schema.HTable ( HTable )
import Rel8.Internal.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Internal.Schema.HTable.Nullify ( HNullify, hnulls, hnullify, hunnullify )
import Rel8.Internal.Schema.HTable.Product ( HProduct( HProduct ) )
import Rel8.Internal.Schema.Null ( Nullify )
import Rel8.Internal.Schema.Spec ( Spec )
import qualified Rel8.Internal.Schema.Kind as K
import Rel8.Internal.Type.Tag ( ADTTagFor, HasConstructor )


type Null :: K.Context -> Type
type Null context = forall a. Spec a -> context (Nullify a)


type Nullifier :: K.Context -> Type
type Nullifier context = forall a. Spec a -> context a -> context (Nullify a)


type Unnullifier :: K.Context -> Type
type Unnullifier context = forall a. Spec a -> context (Nullify a) -> context a


type NoConstructor :: Symbol -> Symbol -> ErrorMessage
type NoConstructor datatype constructor =
  ( 'Text "The type `" ' :<>:
    'Text datatype ' :<>:
    'Text "` has no constructor `" ' :<>:
    'Text constructor ' :<>:
    'Text "`."
  )


type GConstructorADT :: Symbol -> (Type -> Type) -> Type -> Type
type family GConstructorADT name rep where
  GConstructorADT name (M1 D ('MetaData datatype _ _ _) rep) =
    GConstructorADT' name rep (TypeError (NoConstructor datatype name))


type GConstructorADT' :: Symbol -> (Type -> Type) -> (Type -> Type) -> Type -> Type
type family GConstructorADT' name rep fallback where
  GConstructorADT' name (M1 D _ rep) fallback =
    GConstructorADT' name rep fallback
  GConstructorADT' name (a :+: b) fallback =
    GConstructorADT' name a (GConstructorADT' name b fallback)
  GConstructorADT' name (M1 C ('MetaCons name _ _) rep) _ = rep
  GConstructorADT' _ _ fallback = fallback


type GConstructADT
  :: (Type -> Exp Type)
  -> (Type -> Type) -> Type -> Type -> Type
type family GConstructADT f rep r x where
  GConstructADT f (M1 D _ rep) r x = GConstructADT f rep r x
  GConstructADT f (a :+: b) r x = GConstructADT f a r (GConstructADT f b r x)
  GConstructADT f (M1 C _ rep) r x = GConstruct f rep r -> x


type GConstructors :: (Type -> Exp Type) -> (Type -> Type) -> Type -> Type
type family GConstructors f rep where
  GConstructors f (M1 D _ rep) = GConstructors f rep
  GConstructors f (a :+: b) = GConstructors f a :*: GConstructors f b
  GConstructors f (M1 C _ rep) = (->) (GFields f rep)


type RepresentableConstructors :: (Type -> Exp Type) -> (Type -> Type) -> Constraint
class RepresentableConstructors f rep where
  gctabulate :: (GConstructors f rep r -> a) -> GConstructADT f rep r a
  gcindex :: GConstructADT f rep r a -> GConstructors f rep r -> a


instance RepresentableConstructors f rep => RepresentableConstructors f (M1 D meta rep) where
  gctabulate = gctabulate @f @rep
  gcindex = gcindex @f @rep


instance (RepresentableConstructors f a, RepresentableConstructors f b) =>
  RepresentableConstructors f (a :+: b)
 where
  gctabulate f =
    gctabulate @f @a \a -> gctabulate @f @b \b -> f (a :*: b)
  gcindex f (a :*: b) = gcindex @f @b (gcindex @f @a f a) b


instance Representable f rep => RepresentableConstructors f (M1 C meta rep) where
  gctabulate f = f . gindex @f @rep
  gcindex f = f . gtabulate @f @rep


type GBuildADT :: (Type -> Exp Type) -> (Type -> Type) -> Type -> Type
type family GBuildADT f rep r where
  GBuildADT f (M1 D _ rep) r = GBuildADT f rep r
  GBuildADT f (a :+: b) r = GBuildADT f a (GBuildADT f b r)
  GBuildADT f (M1 C _ rep) r = GConstruct f rep r


type GFieldsADT :: (Type -> Exp Type) -> (Type -> Type) -> Type
type family GFieldsADT f rep where
  GFieldsADT f (M1 D _ rep) = GFieldsADT f rep
  GFieldsADT f (a :+: b) = (GFieldsADT f a, GFieldsADT f b)
  GFieldsADT f (M1 C _ rep) = GFields f rep


type RepresentableFields :: (Type -> Exp Type) -> (Type -> Type) -> Constraint
class RepresentableFields f rep where
  gftabulate :: (GFieldsADT f rep -> a) -> GBuildADT f rep a
  gfindex :: GBuildADT f rep a -> GFieldsADT f rep -> a


instance RepresentableFields f rep => RepresentableFields f (M1 D meta rep) where
  gftabulate = gftabulate @f @rep
  gfindex = gfindex @f @rep


instance (RepresentableFields f a, RepresentableFields f b) => RepresentableFields f (a :+: b) where
  gftabulate f =
    gftabulate @f @a \a -> gftabulate @f @b \b -> f (a, b)
  gfindex f (a, b) = gfindex @f @b (gfindex @f @a f a) b


instance Representable f rep => RepresentableFields f (M1 C meta rep) where
  gftabulate = gtabulate @f @rep
  gfindex = gindex @f @rep


type GConstructableADT
  :: Type
  -> (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Exp Type)
  -> K.Context -> (Type -> Type) -> Constraint
class GConstructableADT tag _Table _Columns f context rep where
  gbuildADT :: ()
    => ToColumns _Table _Columns f context
    -> Nullifier context
    -> GTagColumns tag context
    -> GFieldsADT f rep
    -> GColumnsADT tag _Columns rep context

  gunbuildADT :: ()
    => FromColumns _Table _Columns f context
    -> Unnullifier context
    -> GColumnsADT tag _Columns rep context
    -> (GTagColumns tag context, GFieldsADT f rep)

  gconstructADT :: context ~ Expr
    => ToColumns _Table _Columns f context
    -> Null context
    -> Nullifier context
    -> GConstructors f rep (GColumnsADT tag _Columns rep context)

  -- | Each constructor comes with the SQL test of whether a row holds it,
  -- given the row's 'GTagColumns'.
  gdeconstructADT :: context ~ Expr
    => FromColumns _Table _Columns f context
    -> Unnullifier context
    -> GConstructors f rep r
    -> GColumnsADT tag _Columns rep context
    -> (GTagColumns tag context, NonEmpty (GTagColumns tag Expr -> Expr Bool, r))


instance
  ( htable ~ GTagColumns tag
  , GConstructableADT' tag _Table _Columns f context htable rep
  )
  => GConstructableADT tag _Table _Columns f context (M1 D meta rep)
 where
  gbuildADT =
    gbuildADT' @tag @_Table @_Columns @f @context @htable @rep

  gunbuildADT =
    gunbuildADT' @tag @_Table @_Columns @f @context @htable @rep

  gconstructADT toColumns null nullifier =
    gconstructADT' @tag @_Table @_Columns @f @context @htable @rep toColumns null nullifier id

  gdeconstructADT =
    gdeconstructADT' @tag @_Table @_Columns @f @context @htable @rep


type GConstructableADT'
  :: Type
  -> (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Exp Type)
  -> K.Context -> K.HTable -> (Type -> Type) -> Constraint
class GConstructableADT' tag _Table _Columns f context htable rep where
  gbuildADT' :: ()
    => ToColumns _Table _Columns f context
    -> Nullifier context
    -> htable context
    -> GFieldsADT f rep
    -> GColumnsADT' _Columns htable rep context

  gunbuildADT' :: ()
    => FromColumns _Table _Columns f context
    -> Unnullifier context
    -> GColumnsADT' _Columns htable rep context
    -> (htable context, GFieldsADT f rep)

  gconstructADT' :: context ~ Expr
    => ToColumns _Table _Columns f context
    -> Null context
    -> Nullifier context
    -> (GTagColumns tag context -> htable context)
    -> GConstructors f rep (GColumnsADT' _Columns htable rep context)

  gdeconstructADT' :: context ~ Expr
    => FromColumns _Table _Columns f context
    -> Unnullifier context
    -> GConstructors f rep r
    -> GColumnsADT' _Columns htable rep context
    -> (htable context, NonEmpty (GTagColumns tag Expr -> Expr Bool, r))

  gfill :: ()
    => Null context
    -> htable context
    -> GColumnsADT' _Columns htable rep context


instance
  ( htable' ~ GColumnsADT' _Columns htable a
  , Functor (GConstructors f a)
  , GConstructableADT' tag _Table _Columns f context htable a
  , GConstructableADT' tag _Table _Columns f context htable' b
  )
  => GConstructableADT' tag _Table _Columns f context htable (a :+: b)
 where
  gbuildADT' toColumns nullifier htable (a, b) =
    gbuildADT' @tag @_Table @_Columns @f @context @htable' @b toColumns nullifier
      (gbuildADT' @tag @_Table @_Columns @f @context @htable @a toColumns nullifier htable a)
      b

  gunbuildADT' fromColumns unnullifier columns =
    case gunbuildADT' @tag @_Table @_Columns @f @context @htable' @b fromColumns unnullifier columns of
      (htable', b) ->
        case gunbuildADT' @tag @_Table @_Columns @f @context @htable @a fromColumns unnullifier htable' of
          (htable, a) -> (htable, (a, b))

  gconstructADT' toColumns null nullifier mk =
    fmap (gfill @tag @_Table @_Columns @f @context @htable' @b null) (gconstructADT' @tag @_Table @_Columns @f @context @htable @a toColumns null nullifier mk) :*:
    gconstructADT' @tag @_Table @_Columns @f @context @htable' @b toColumns null nullifier (gfill @tag @_Table @_Columns @f @context @htable @a null . mk)

  gdeconstructADT' fromColumns unnullifier (a :*: b) columns =
    case gdeconstructADT' @tag @_Table @_Columns @f @context @htable' @b fromColumns unnullifier b columns of
      (htable', cases) ->
        case gdeconstructADT' @tag @_Table @_Columns @f @context @htable @a fromColumns unnullifier a htable' of
          (htable, cases') -> (htable, cases' <> cases)

  gfill null =
    gfill @tag @_Table @_Columns @f @context @htable' @b null .
    gfill @tag @_Table @_Columns @f @context @htable @a null


instance (meta ~ 'MetaCons label _fixity _isRecord, ADTTagFor tag label) =>
  GConstructableADT' tag _Table _Columns f context htable (M1 C meta U1)
 where
  gbuildADT' _ _ = const
  gunbuildADT' _ _ = (, ())
  gconstructADT' _ _ _ f _ = f (gtagExpr @tag @label)
  gdeconstructADT' _ _ r htable = (htable, pure (gisTagExpr @tag @label, r ()))
  gfill _ = id


instance {-# OVERLAPPABLE #-}
  ( HTable (GColumns _Columns rep)
  , meta ~ 'MetaCons label _fixity _isRecord
  , GConstructable _Table _Columns f context rep
  , ADTTagFor tag label
  , GColumnsADT' _Columns htable (M1 C meta rep) ~
      HProduct htable (HLabel label (HNullify (GColumns _Columns rep)))
  )
  => GConstructableADT' tag _Table _Columns f context htable (M1 C meta rep)
 where
  gbuildADT' toColumns nullifier htable =
    HProduct htable .
    hlabel .
    hnullify nullifier .
    gconstruct @_Table @_Columns @f @context @rep toColumns

  gunbuildADT' fromColumns unnullifier (HProduct htable a) =
    ( htable
    , gdeconstruct @_Table @_Columns @f @context @rep fromColumns $
        runIdentity $
        hunnullify (\spec -> pure . unnullifier spec) $
        hunlabel
        a
    )

  gconstructADT' toColumns _ nullifier mk =
    HProduct htable .
    hlabel .
    hnullify nullifier .
    gconstruct @_Table @_Columns @f @context @rep toColumns
    where
      htable = mk (gtagExpr @tag @label)

  gdeconstructADT' fromColumns unnullifier r (HProduct htable columns) =
    ( htable
    , pure (gisTagExpr @tag @label, r a)
    )
    where
      a = gdeconstruct @_Table @_Columns @f @context @rep fromColumns $
        runIdentity $
        hunnullify (\spec -> pure . unnullifier spec) $
        hunlabel
        columns

  gfill null htable = HProduct htable (hlabel (hnulls null))


type GMakeableADT
  :: Type
  -> (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Exp Type)
  -> K.Context -> Symbol -> (Type -> Type) -> Constraint
class GMakeableADT tag _Table _Columns f context name rep where
  gmakeADT :: context ~ Expr
    => ToColumns _Table _Columns f context
    -> Null context
    -> Nullifier context
    -> GFields f (GConstructorADT name rep)
    -> GColumnsADT tag _Columns rep context


instance
  ( htable ~ GTagColumns tag
  , meta ~ 'MetaData datatype _module _package _newtype
  , fallback ~ TypeError (NoConstructor datatype name)
  , fields ~ GFields f (GConstructorADT' name rep fallback)
  , GMakeableADT' tag _Table _Columns f context htable name rep fields
  , MakeTag (HasConstructor name rep) tag name
  )
  => GMakeableADT tag _Table _Columns f context name (M1 D meta rep)
 where
  gmakeADT toColumns null nullifier =
    gmakeADT'
      @tag @_Table @_Columns @f @context @htable @name @rep @fields
      toColumns null nullifier
      (makeTag @(HasConstructor name rep) @tag @name)


type MakeTag :: Bool -> Type -> Symbol -> Constraint
class MakeTag found tag name where
  makeTag :: GTagColumns tag Expr


instance ADTTagFor tag name => MakeTag 'True tag name where
  makeTag = gtagExpr @tag @name


instance MakeTag 'False tag name where
  makeTag = error "unreachable: NoConstructor is a type error"


type GMakeableADT'
  :: Type
  -> (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Exp Type)
  -> K.Context -> K.HTable -> Symbol -> (Type -> Type) -> Type -> Constraint
class GMakeableADT' tag _Table _Columns f context htable name rep fields where
  gmakeADT' :: ()
    => ToColumns _Table _Columns f context
    -> Null context
    -> Nullifier context
    -> htable context
    -> fields
    -> GColumnsADT' _Columns htable rep context


instance
  ( htable' ~ GColumnsADT' _Columns htable a
  , GMakeableADT' tag _Table _Columns f context htable name a fields
  , GMakeableADT' tag _Table _Columns f context htable' name b fields
  )
  => GMakeableADT' tag _Table _Columns f context htable name (a :+: b) fields
 where
  gmakeADT' toColumns null nullifier htable x =
    gmakeADT' @tag @_Table @_Columns @f @context @htable' @name @b @fields
      toColumns null nullifier
      (gmakeADT'
         @tag @_Table @_Columns @f @context @htable @name @a @fields toColumns
         null nullifier htable x)
      x


instance {-# OVERLAPPING #-}
  GMakeableADT' tag _Table _Columns f context htable name (M1 C ('MetaCons name _fixity _isRecord) U1) fields
 where
  gmakeADT' _ _ _ = const


instance {-# OVERLAPS #-}
  GMakeableADT' tag _Table _Columns f context htable name (M1 C ('MetaCons label _fixity _isRecord) U1) fields
 where
  gmakeADT' _ _ _ = const


instance {-# OVERLAPS #-}
  ( HTable (GColumns _Columns rep)
  , GConstructable _Table _Columns f context rep
  , fields ~ GFields f rep
  , GColumnsADT' _Columns htable (M1 C ('MetaCons name _fixity _isRecord) rep) ~
      HProduct htable (HLabel name (HNullify (GColumns _Columns rep)))
  )
  => GMakeableADT' tag _Table _Columns f context htable name (M1 C ('MetaCons name _fixity _isRecord) rep) fields
 where
  gmakeADT' toColumns _ nullifier htable =
    HProduct htable .
    hlabel .
    hnullify nullifier .
    gconstruct @_Table @_Columns @f @context @rep toColumns


instance {-# OVERLAPPABLE #-}
  ( HTable (GColumns _Columns rep)
  , GColumnsADT' _Columns htable (M1 C ('MetaCons label _fixity _isRecord) rep) ~
      HProduct htable (HLabel label (HNullify (GColumns _Columns rep)))
  )
  => GMakeableADT' tag _Table _Columns f context htable name (M1 C ('MetaCons label _fixity _isRecord) rep) fields
 where
  gmakeADT' _ null _ htable _ =
    HProduct htable $
    hlabel $
    hnulls null
