{-# language DataKinds #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}

module Rel8.Type.Array
  ( Array(..), array
  , arrayTypeInformation
  )
where

-- aeson
import Data.Aeson
  ( FromJSON, parseJSON
  , ToJSON, toEncoding, toJSON
  )

-- base
import Control.Applicative ( Alternative, (<|>), empty, liftA2 )
import Control.Monad ( MonadPlus )
import Data.Bifunctor ( first )
import Data.Foldable ( toList )
import Data.Functor.Compose ( Compose( Compose ), getCompose )
import Data.Kind ( Type )
import Data.List.NonEmpty ( NonEmpty, nonEmpty )
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Exts ( IsList, Item )
import qualified GHC.Exts
import Prelude hiding ( null, repeat, zipWith )

-- hasql
import qualified Hasql.Decoders as Hasql

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye

-- rel8
import Rel8.Expr ( Expr( Expr ) )
import Rel8.Kind.Emptiability
  ( Emptiability( Emptiable, NonEmptiable )
  , SEmptiability( SEmptiable, SNonEmptiable )
  , KnownEmptiability
  , emptiabilitySing
  )
import Rel8.Kind.Nullability
  ( Nullability( Nullable, NonNullable )
  , SNullability( SNullable, SNonNullable )
  , KnownNullability
  , nullabilitySing
  )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Type ( DBType, typeInformation, TypeInformation(..) )
import Rel8.Type.Eq ( DBEq )
import Rel8.Type.Monoid ( DBMonoid, memptyExpr )
import Rel8.Type.Ord ( DBOrd )
import Rel8.Type.Semigroup ( DBSemigroup, (<>.) )

-- semialign
import Data.Align ( Align, nil )
import Data.Semialign ( Semialign, alignWith )
import Data.Zip ( Zip, zipWith, Repeat, repeat )

-- semigroupoids
import Data.Functor.Apply ( Apply, liftF2 )
import Data.Functor.Alt ( Alt, (<!>) )
import Data.Functor.Bind ( Bind, (>>-) )
import Data.Functor.Plus ( Plus, zero )
import Data.Semigroup.Foldable ( Foldable1, foldMap1 )
import Data.Semigroup.Traversable ( Traversable1, traverse1 )

-- transformers
import Control.Monad.Trans.Maybe ( MaybeT( MaybeT ), runMaybeT )


type Array :: Emptiability -> Nullability -> Type -> Type
data Array emptiability nullability a where
  NullableList :: [Maybe a] -> Array 'Emptiable 'Nullable a
  NullableNonEmpty :: NonEmpty (Maybe a) -> Array 'NonEmptiable 'Nullable a
  NonNullableList :: [a] -> Array 'Emptiable 'NonNullable a
  NonNullableNonEmpty :: NonEmpty a -> Array 'NonEmptiable 'NonNullable a


instance Functor (Array emptiability nullability) where
  fmap f = \case
    NullableList as -> NullableList (fmap (fmap f) as)
    NullableNonEmpty as -> NullableNonEmpty (fmap (fmap f) as)
    NonNullableList as -> NonNullableList (fmap f as)
    NonNullableNonEmpty as -> NonNullableNonEmpty (fmap f as)


instance Foldable (Array emptiability nullability) where
  foldMap f = \case
    NullableList as -> foldMap (foldMap f) as
    NullableNonEmpty as -> foldMap (foldMap f) as
    NonNullableList as -> foldMap f as
    NonNullableNonEmpty as -> foldMap f as


instance Traversable (Array emptiability nullability) where
  traverse f = \case
    NullableList as -> NullableList <$> traverse (traverse f) as
    NullableNonEmpty as -> NullableNonEmpty <$> traverse (traverse f) as
    NonNullableList as -> NonNullableList <$> traverse f as
    NonNullableNonEmpty as -> NonNullableNonEmpty <$> traverse f as


instance (emptiability ~ 'NonEmptiable, nullability ~ 'NonNullable) =>
  Foldable1 (Array emptiability nullability)
 where
   foldMap1 f = \case
     NonNullableNonEmpty as -> foldMap1 f as


instance (emptiability ~ 'NonEmptiable, nullability ~ 'NonNullable) =>
  Traversable1 (Array emptiability nullability)
 where
   traverse1 f = \case
     NonNullableNonEmpty as -> NonNullableNonEmpty <$> traverse1 f as


instance Semialign (Array emptiability nullability) where
  alignWith f (NullableList as) (NullableList bs) =
    NullableList $ getCompose $ alignWith f (Compose as) (Compose bs)
  alignWith f (NullableNonEmpty as) (NullableNonEmpty bs) =
    NullableNonEmpty $ getCompose $ alignWith f (Compose as) (Compose bs)
  alignWith f (NonNullableList as) (NonNullableList bs) =
    NonNullableList $ alignWith f as bs
  alignWith f (NonNullableNonEmpty as) (NonNullableNonEmpty bs) =
    NonNullableNonEmpty $ alignWith f as bs


instance Zip (Array emptiability nullability) where
  zipWith f (NullableList as) (NullableList bs) =
    NullableList $ getCompose $ zipWith f (Compose as) (Compose bs)
  zipWith f (NullableNonEmpty as) (NullableNonEmpty bs) =
    NullableNonEmpty $ getCompose $ zipWith f (Compose as) (Compose bs)
  zipWith f (NonNullableList as) (NonNullableList bs) =
    NonNullableList $ zipWith f as bs
  zipWith f (NonNullableNonEmpty as) (NonNullableNonEmpty bs) =
    NonNullableNonEmpty $ zipWith f as bs


instance (KnownEmptiability emptiability, KnownNullability nullability) =>
  Repeat (Array emptiability nullability)
 where
  repeat = case (emptiability, nullability) of
    (SEmptiable, SNullable) -> NullableList . repeat . pure
    (SNonEmptiable, SNullable) -> NullableNonEmpty . repeat . pure
    (SEmptiable, SNonNullable) -> NonNullableList . repeat
    (SNonEmptiable, SNonNullable) -> NonNullableNonEmpty . repeat
    where
      emptiability = emptiabilitySing @emptiability
      nullability = nullabilitySing @nullability


instance (emptiability ~ 'Emptiable, KnownNullability nullability) =>
  Align (Array emptiability nullability)
 where
  nil = case nullability of
    SNullable -> NullableList []
    SNonNullable -> NonNullableList []
    where
      nullability = nullabilitySing @nullability


instance Apply (Array emptiability nullability) where
  liftF2 f (NullableList as) (NullableList bs) =
    NullableList (liftF2 (liftF2 f) as bs)
  liftF2 f (NullableNonEmpty as) (NullableNonEmpty bs) =
    NullableNonEmpty (liftF2 (liftF2 f) as bs)
  liftF2 f (NonNullableList as) (NonNullableList bs) =
    NonNullableList (liftF2 f as bs)
  liftF2 f (NonNullableNonEmpty as) (NonNullableNonEmpty bs) =
    NonNullableNonEmpty (liftF2 f as bs)


instance Alt (Array emptiability nullability) where
  NullableList as <!> NullableList bs = NullableList (as <!> bs)
  NullableNonEmpty as <!> NullableNonEmpty bs = NullableNonEmpty (as <!> bs)
  NonNullableList as <!> NonNullableList bs = NonNullableList (as <!> bs)
  NonNullableNonEmpty as <!> NonNullableNonEmpty bs =
    NonNullableNonEmpty (as <!> bs)


instance Bind (Array emptiability nullability) where
  NullableList as >>- f = NullableList $ runMaybeT $ MaybeT as >>-
    \a -> case f a of
      NullableList bs -> MaybeT bs
  NullableNonEmpty as >>- f = NullableNonEmpty $ runMaybeT $ MaybeT as >>-
    \a -> case f a of
      NullableNonEmpty bs -> MaybeT bs
  NonNullableList as >>- f = NonNullableList $ as >>-
    \a -> case f a of
      NonNullableList bs -> bs
  NonNullableNonEmpty as >>- f = NonNullableNonEmpty $ as >>-
    \a -> case f a of
      NonNullableNonEmpty bs -> bs


instance (KnownEmptiability emptiability, KnownNullability nullability) =>
  Applicative (Array emptiability nullability)
 where
  liftA2 = liftF2
  pure a = case (emptiability, nullability) of
    (SEmptiable, SNullable) -> NullableList (pure (pure a))
    (SNonEmptiable, SNullable) -> NullableNonEmpty (pure (pure a))
    (SEmptiable, SNonNullable) -> NonNullableList (pure a)
    (SNonEmptiable, SNonNullable) -> NonNullableNonEmpty (pure a)
    where
      emptiability = emptiabilitySing @emptiability
      nullability = nullabilitySing @nullability


instance (emptiability ~ 'Emptiable, KnownNullability nullability) =>
  Plus (Array emptiability nullability)
 where
  zero = case nullabilitySing @nullability of
    SNullable -> NullableList []
    SNonNullable -> NonNullableList []


instance (emptiability ~ 'Emptiable, KnownNullability nullability) =>
  Alternative (Array emptiability nullability)
 where
  empty = zero
  (<|>) = (<!>)


instance (KnownEmptiability emptiability, KnownNullability nullability) =>
  Monad (Array emptiability nullability)
 where
  (>>=) = (>>-)


instance (emptiability ~ 'Emptiable, KnownNullability nullability) =>
  MonadPlus (Array emptiability nullability)


instance Eq a => Eq (Array emptiability nullability a) where
  NullableList a == NullableList b = a == b
  NullableNonEmpty a == NullableNonEmpty b = a == b
  NonNullableList a == NonNullableList b = a == b
  NonNullableNonEmpty a == NonNullableNonEmpty b = a == b


instance Ord a => Ord (Array emptiability nullability a) where
  compare (NullableList a) (NullableList b) = compare a b
  compare (NullableNonEmpty a) (NullableNonEmpty b) = compare a b
  compare (NonNullableList a) (NonNullableList b) = compare a b
  compare (NonNullableNonEmpty a) (NonNullableNonEmpty b) = compare a b


instance
  ( KnownEmptiability emptiability
  , KnownNullability nullability
  , Read a
  ) => Read (Array emptiability nullability a)
 where
  readsPrec _ s = case (emptiability, nullability) of
    (SEmptiable, SNullable) -> first NullableList <$> readList s
    (SNonEmptiable, SNullable) -> do
      (as, s') <- readList s
      as' <- maybe empty pure $ nonEmpty as
      pure (NullableNonEmpty as', s')
    (SEmptiable, SNonNullable) -> first NonNullableList <$> readList s
    (SNonEmptiable, SNonNullable) -> do
      (as, s') <- readList s
      as' <- maybe empty pure $ nonEmpty as
      pure (NonNullableNonEmpty as', s')
    where
      emptiability = emptiabilitySing @emptiability
      nullability = nullabilitySing @nullability


instance Show a => Show (Array emptiability nullability a) where
  showsPrec _ (NullableList as) = showList as
  showsPrec _ (NullableNonEmpty as) = showList (toList as)
  showsPrec _ (NonNullableList as) = showList as
  showsPrec _ (NonNullableNonEmpty as) = showList (toList as)


instance Semigroup (Array emptiability nullability a) where
  (<>) = (<!>)


instance (emptiability ~ 'Emptiable, KnownNullability nullability) =>
  Monoid (Array emptiability nullability a)
 where
  mempty = zero


instance
  ( KnownEmptiability emptiability
  , KnownNullability nullability
  , FromJSON a
  ) => FromJSON (Array emptiability nullability a)
 where
  parseJSON a = case (emptiability, nullability) of
    (SEmptiable, SNullable) -> NullableList <$> parseJSON a
    (SNonEmptiable, SNullable) -> NullableNonEmpty <$> parseJSON a
    (SEmptiable, SNonNullable) -> NonNullableList <$> parseJSON a
    (SNonEmptiable, SNonNullable) -> NonNullableNonEmpty <$> parseJSON a
    where
      emptiability = emptiabilitySing @emptiability
      nullability = nullabilitySing @nullability


instance ToJSON a => ToJSON (Array emptiability nullability a) where
  toEncoding = \case
    NullableList as -> toEncoding as
    NullableNonEmpty as -> toEncoding as
    NonNullableList as -> toEncoding as
    NonNullableNonEmpty as -> toEncoding as
  toJSON = \case
    NullableList as -> toJSON as
    NullableNonEmpty as -> toJSON as
    NonNullableList as -> toJSON as
    NonNullableNonEmpty as -> toJSON as


type ArrayItem :: Nullability -> Type -> Type
type family ArrayItem nullability a where
  ArrayItem 'Nullable a = Maybe a
  ArrayItem 'NonNullable a = a


instance (KnownEmptiability emptiability, KnownNullability nullability) =>
  IsList (Array emptiability nullability a)
 where
  type Item (Array emptiability nullability a) = ArrayItem nullability a

  fromList = case (emptiability, nullability) of
    (SEmptiable, SNullable) -> NullableList
    (SNonEmptiable, SNullable) -> NullableNonEmpty . NonEmpty.fromList
    (SEmptiable, SNonNullable) -> NonNullableList
    (SNonEmptiable, SNonNullable) -> NonNullableNonEmpty . NonEmpty.fromList
    where
      emptiability = emptiabilitySing @emptiability
      nullability = nullabilitySing @nullability

  toList = \case
    NullableList as -> as
    NullableNonEmpty as -> toList as
    NonNullableList as -> as
    NonNullableNonEmpty as -> toList as


instance
  ( KnownEmptiability emptiability
  , KnownNullability nullability
  , DBType a
  ) => DBType (Array emptiability nullability a)
 where
  typeInformation =
    arrayTypeInformation emptiabilitySing nullabilitySing typeInformation


instance
  ( KnownEmptiability emptiability
  , KnownNullability nullability
  , DBEq a
  ) => DBEq (Array emptiability nullability a)


instance
  ( KnownEmptiability emptiability
  , KnownNullability nullability
  , DBOrd a
  ) => DBOrd (Array emptiability nullability a)


instance
  ( KnownEmptiability emptiability
  , KnownNullability nullability
  , DBType a
  ) => DBSemigroup (Array emptiability nullability a)
 where
  (<>.) = (++.)


instance
  ( KnownEmptiability emptiability
  , KnownNullability nullability
  , DBType a
  ) => DBMonoid (Array emptiability nullability a)
 where
  memptyExpr = Expr (array (typeInformation @a) [])


array :: Foldable f
  => TypeInformation a -> f Opaleye.PrimExpr -> Opaleye.PrimExpr
array TypeInformation {typeName} =
  Opaleye.FunExpr "ROW" . pure .
  Opaleye.CastExpr (typeName <> "[]") .
  Opaleye.ArrayExpr . toList


arrayTypeInformation :: ()
  => SEmptiability emptiability
  -> SNullability nullability
  -> TypeInformation a
  -> TypeInformation (Array emptiability nullability a)
arrayTypeInformation emptiability nullability info = TypeInformation
  { decode = row $ case (emptiability, nullability) of
      (SEmptiable, SNullable) -> NullableList <$>
        Hasql.listArray (Hasql.nullable decode)
      (SNonEmptiable, SNullable) -> NullableNonEmpty <$>
        nonEmptyArray (Hasql.nullable decode)
      (SEmptiable, SNonNullable) -> NonNullableList <$>
        Hasql.listArray (Hasql.nonNullable decode)
      (SNonEmptiable, SNonNullable) -> NonNullableNonEmpty <$>
        nonEmptyArray (Hasql.nonNullable decode)
  , encode = \case
      NullableList as -> array info (maybe null encode <$> as)
      NonNullableList as -> array info (encode <$> as)
      NullableNonEmpty as -> array info (maybe null encode <$> as)
      NonNullableNonEmpty as -> array info (encode <$> as)
  , typeName = "record"
  , typeable = case typeable of
      Dict -> case (emptiability, nullability) of
        (SEmptiable, SNullable) -> Dict
        (SNonEmptiable, SNullable) -> Dict
        (SEmptiable, SNonNullable) -> Dict
        (SNonEmptiable, SNonNullable) -> Dict
  }
  where
    TypeInformation {encode, decode, typeable} = info
    row = Hasql.composite . Hasql.field . Hasql.nonNullable
    nonEmptyArray =
      Hasql.refine (maybe (Left message) Right . nonEmpty) . Hasql.listArray
      where
        message = "failed to decode NonEmptiable Array: empty list"
    null = Opaleye.ConstExpr Opaleye.NullLit


(++.) :: ()
  => Expr 'NonNullable (Array emptiability nullability a)
  -> Expr 'NonNullable (Array emptiability nullability a)
  -> Expr 'NonNullable (Array emptiability nullability a)
Expr a ++. Expr b = Expr (Opaleye.BinExpr (Opaleye.:||) (unrow a) (unrow b))
infixr 5 ++.


unrow :: Opaleye.PrimExpr -> Opaleye.PrimExpr
unrow a = Opaleye.CompositeExpr a "f1"
