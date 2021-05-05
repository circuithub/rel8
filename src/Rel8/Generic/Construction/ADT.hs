{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language DataKinds #-}
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

module Rel8.Generic.Construction.ADT
  ( GConstructableADT
  , GBuildADT, gbuildADT, gunbuildADT
  , GConstructADT, gconstructADT, gdeconstructADT
  , GFields, CorepFields, gftabulate, gfindex, gftraverse
  , GConstructors, CorepConstructors, gctabulate, gcindex
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.Bifunctor ( first )
import Data.Functor.Identity ( runIdentity )
import Data.Kind ( Constraint, Type )
import Data.List.NonEmpty ( NonEmpty )
import Data.Proxy ( Proxy( Proxy ) )
import GHC.Generics
  ( (:+:), (:*:)( (:*:) ), M1, U1
  , C, D
  , Meta( MetaCons )
  )
import GHC.TypeLits ( KnownSymbol, symbolVal )
import Prelude hiding ( null )

-- rel8
import Rel8.FCF ( Compose, Exp )
import Rel8.Generic.Construction.Record
  ( GConstruct, GConstructable, gconstruct, gdeconstruct
  , GFields, Corep, gtabulate, gindex, gtraverse
  , FromColumns, ToColumns
  )
import Rel8.Generic.Table.ADT ( GColumnsADT, GColumnsADT' )
import Rel8.Generic.Table.Record ( GColumns )
import Rel8.Schema.Context.Label ( HLabelable, hlabeler, hunlabeler )
import Rel8.Schema.HTable ( HTable )
import Rel8.Schema.HTable.Identity ( HType )
import Rel8.Schema.HTable.Label ( HLabel, hlabel, hunlabel )
import Rel8.Schema.HTable.Nullify ( HNullify, hnulls, hnullify, hunnullify )
import Rel8.Schema.HTable.Product ( HProduct( HProduct ) )
import Rel8.Schema.Null ( Nullify )
import Rel8.Schema.Spec ( Spec( Spec ), SSpec )
import qualified Rel8.Schema.Kind as K
import Rel8.Type.Tag ( Tag( Tag ) )

-- text
import Data.Text ( pack )


type Null :: K.HContext -> Type
type Null context = forall labels necessity a. ()
  => SSpec ('Spec labels necessity a)
  -> context ('Spec labels necessity (Nullify a))


type Nullifier :: K.HContext -> Type
type Nullifier context = forall labels necessity a. ()
  => SSpec ('Spec labels necessity a)
  -> context ('Spec labels necessity a)
  -> context ('Spec labels necessity (Nullify a))


type Unnullifier :: K.HContext -> Type
type Unnullifier context = forall labels necessity a. ()
  => SSpec ('Spec labels necessity a)
  -> context ('Spec labels necessity (Nullify a))
  -> context ('Spec labels necessity a)


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


type CorepConstructors :: (Type -> Exp Type) -> (Type -> Type) -> Constraint
class CorepConstructors f rep where
  gctabulate :: (GConstructors f rep r -> a) -> GConstructADT f rep r a
  gcindex :: GConstructADT f rep r a -> GConstructors f rep r -> a


instance CorepConstructors f rep => CorepConstructors f (M1 D meta rep) where
  gctabulate = gctabulate @f @rep
  gcindex = gcindex @f @rep


instance (CorepConstructors f a, CorepConstructors f b) =>
  CorepConstructors f (a :+: b)
 where
  gctabulate f =
    gctabulate @f @a \a -> gctabulate @f @b \b -> f (a :*: b)
  gcindex f (a :*: b) = gcindex @f @b (gcindex @f @a f a) b


instance Corep f rep => CorepConstructors f (M1 C meta rep) where
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


type CorepFields :: (Type -> Exp Type) -> (Type -> Type) -> Constraint
class CorepFields f rep where
  gftabulate :: (GFieldsADT f rep -> a) -> GBuildADT f rep a
  gfindex :: GBuildADT f rep a -> GFieldsADT f rep -> a
  gftraverse :: Applicative m
    => (forall a. g a -> m a)
    -> GFieldsADT (Compose g f) rep -> m (GFieldsADT f rep)


instance CorepFields f rep => CorepFields f (M1 D meta rep) where
  gftabulate = gftabulate @f @rep
  gfindex = gfindex @f @rep
  gftraverse = gftraverse @f @rep


instance (CorepFields f a, CorepFields f b) => CorepFields f (a :+: b) where
  gftabulate f =
    gftabulate @f @a \a -> gftabulate @f @b \b -> f (a, b)
  gfindex f (a, b) = gfindex @f @b (gfindex @f @a f a) b
  gftraverse f (a, b) = liftA2 (,) (gftraverse @f @a f a) (gftraverse @f @b f b)


instance Corep f rep => CorepFields f (M1 C meta rep) where
  gftabulate = gtabulate @f @rep
  gfindex = gindex @f @rep
  gftraverse = gtraverse @f @rep


type GConstructableADT
  :: (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Exp Type)
  -> K.HContext -> (Type -> Type) -> Constraint
class GConstructableADT _Table _Columns f context rep where
  gbuildADT :: ()
    => ToColumns _Table _Columns f context
    -> (Tag -> Nullifier context)
    -> HType Tag context
    -> GFieldsADT f rep
    -> GColumnsADT _Columns rep context

  gunbuildADT :: ()
    => FromColumns _Table _Columns f context
    -> Unnullifier context
    -> GColumnsADT _Columns rep context
    -> (HType Tag context, GFieldsADT f rep)

  gconstructADT :: ()
    => ToColumns _Table _Columns f context
    -> Null context
    -> Nullifier context
    -> (Tag -> HType Tag context)
    -> GConstructors f rep (GColumnsADT _Columns rep context)

  gdeconstructADT :: ()
    => FromColumns _Table _Columns f context
    -> Unnullifier context
    -> GConstructors f rep r
    -> GColumnsADT _Columns rep context
    -> (HType Tag context, NonEmpty (Tag, r))


instance
  ( htable ~ HLabel "tag" (HType Tag)
  , GConstructableADT' _Table _Columns f context htable rep
  , HLabelable context
  )
  => GConstructableADT _Table _Columns f context (M1 D meta rep)
 where
  gbuildADT toColumns nullifier =
    gbuildADT' @_Table @_Columns @f @context @htable @rep toColumns nullifier .
    hlabel hlabeler

  gunbuildADT fromColumns unnullifier =
    first (hunlabel hunlabeler) .
    gunbuildADT' @_Table @_Columns @f @context @htable @rep fromColumns unnullifier

  gconstructADT toColumns null nullifier mk =
    gconstructADT' @_Table @_Columns @f @context @htable @rep toColumns null nullifier
      (hlabel hlabeler . mk)

  gdeconstructADT fromColumns unnullifier cases =
    first (hunlabel hunlabeler) .
    gdeconstructADT' @_Table @_Columns @f @context @htable @rep fromColumns unnullifier cases


type GConstructableADT'
  :: (Type -> Exp Constraint)
  -> (Type -> Exp K.HTable)
  -> (Type -> Exp Type)
  -> K.HContext -> K.HTable -> (Type -> Type) -> Constraint
class GConstructableADT' _Table _Columns f context htable rep where
  gbuildADT' :: ()
    => ToColumns _Table _Columns f context
    -> (Tag -> Nullifier context)
    -> htable context
    -> GFieldsADT f rep
    -> GColumnsADT' _Columns htable rep context

  gunbuildADT' :: ()
    => FromColumns _Table _Columns f context
    -> Unnullifier context
    -> GColumnsADT' _Columns htable rep context
    -> (htable context, GFieldsADT f rep)

  gconstructADT' :: ()
    => ToColumns _Table _Columns f context
    -> Null context
    -> Nullifier context
    -> (Tag -> htable context)
    -> GConstructors f rep (GColumnsADT' _Columns htable rep context)

  gdeconstructADT' :: ()
    => FromColumns _Table _Columns f context
    -> Unnullifier context
    -> GConstructors f rep r
    -> GColumnsADT' _Columns htable rep context
    -> (htable context, NonEmpty (Tag, r))

  gfill :: ()
    => Null context
    -> htable context
    -> GColumnsADT' _Columns htable rep context


instance
  ( htable' ~ GColumnsADT' _Columns htable a
  , Functor (GConstructors f a)
  , GConstructableADT' _Table _Columns f context htable a
  , GConstructableADT' _Table _Columns f context htable' b
  )
  => GConstructableADT' _Table _Columns f context htable (a :+: b)
 where
  gbuildADT' toColumns nullifier htable (a, b) =
    gbuildADT' @_Table @_Columns @f @context @htable' @b toColumns nullifier
      (gbuildADT' @_Table @_Columns @f @context @htable @a toColumns nullifier htable a)
      b

  gunbuildADT' fromColumns unnullifier columns =
    case gunbuildADT' @_Table @_Columns @f @context @htable' @b fromColumns unnullifier columns of
      (htable', b) ->
        case gunbuildADT' @_Table @_Columns @f @context @htable @a fromColumns unnullifier htable' of
          (htable, a) -> (htable, (a, b))

  gconstructADT' toColumns null nullifier mk =
    fmap (gfill @_Table @_Columns @f @context @htable' @b null) (gconstructADT' @_Table @_Columns @f @context @htable @a toColumns null nullifier mk) :*:
    gconstructADT' @_Table @_Columns @f @context @htable' @b toColumns null nullifier (gfill @_Table @_Columns @f @context @htable @a null . mk)

  gdeconstructADT' fromColumns unnullifier (a :*: b) columns =
    case gdeconstructADT' @_Table @_Columns @f @context @htable' @b fromColumns unnullifier b columns of
      (htable', cases) ->
        case gdeconstructADT' @_Table @_Columns @f @context @htable @a fromColumns unnullifier a htable' of
          (htable, cases') -> (htable, cases' <> cases)

  gfill null =
    gfill @_Table @_Columns @f @context @htable' @b null .
    gfill @_Table @_Columns @f @context @htable @a null


instance (meta ~ 'MetaCons label _fixity _isRecord, KnownSymbol label) =>
  GConstructableADT' _Table _Columns f context htable (M1 C meta U1)
 where
  gbuildADT' _ _ = const
  gunbuildADT' _ _ = (, ())
  gconstructADT' _ _ _ f _ = f tag
    where
      tag = Tag $ pack $ symbolVal (Proxy @label)
  gdeconstructADT' _ _ r htable = (htable, pure (tag, r ()))
    where
      tag = Tag $ pack $ symbolVal (Proxy @label)
  gfill _ = id


instance {-# OVERLAPPABLE #-}
  ( HTable (GColumns _Columns rep)
  , KnownSymbol label
  , meta ~ 'MetaCons label _fixity _isRecord
  , HLabelable context
  , GConstructable _Table _Columns f context rep
  , GColumnsADT' _Columns htable (M1 C meta rep) ~
      HProduct htable (HLabel label (HNullify (GColumns _Columns rep)))
  )
  => GConstructableADT' _Table _Columns f context htable (M1 C meta rep)
 where
  gbuildADT' toColumns nullifier htable =
    HProduct htable .
    hlabel hlabeler .
    hnullify (nullifier tag) .
    gconstruct @_Table @_Columns @f @context @rep toColumns
    where
      tag = Tag $ pack $ symbolVal (Proxy @label)

  gunbuildADT' fromColumns unnullifier (HProduct htable a) =
    ( htable
    , gdeconstruct @_Table @_Columns @f @context @rep fromColumns $
        runIdentity $
        hunnullify (\spec -> pure . unnullifier spec) $
        hunlabel hunlabeler
        a
    )

  gconstructADT' toColumns _ nullifier mk =
    HProduct htable .
    hlabel hlabeler .
    hnullify nullifier .
    gconstruct @_Table @_Columns @f @context @rep toColumns
    where
      tag = Tag $ pack $ symbolVal (Proxy @label)
      htable = mk tag

  gdeconstructADT' fromColumns unnullifier r (HProduct htable columns) =
    ( htable
    , pure (tag, r a)
    )
    where
      a = gdeconstruct @_Table @_Columns @f @context @rep fromColumns $
        runIdentity $
        hunnullify (\spec -> pure . unnullifier spec) $
        hunlabel hunlabeler
        columns
      tag = Tag $ pack $ symbolVal (Proxy @label)

  gfill null htable = HProduct htable (hlabel hlabeler (hnulls null))
