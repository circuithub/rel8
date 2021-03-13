{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Rel8.Schema.HTable.Vectorize
  ( HVectorize( HVectorize )
  , hvectorize, hunvectorize
  , happend, hempty
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Kind.Emptiability
  ( Emptiability, KnownEmptiability, emptiabilitySing
  )
import Rel8.Kind.Nullability
  ( Nullability( NonNullable )
  , SNullability( SNonNullable )
  )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable
  ( HTable, HConstrainTable, HField
  , hfield, htabulate, htabulateA, htraverse, hdicts, hspecs
  )
import Rel8.Schema.HTable.Context ( H, HKTable )
import Rel8.Schema.HTable.Functor ( HFunctor, hmap )
import Rel8.Schema.Spec
  ( Context
  , Spec( Spec )
  , SSpec( SSpec )
  )
import Rel8.Type ( TypeInformation )
import Rel8.Type.Array ( Array, arrayTypeInformation )

-- semialign
import Data.Zip ( Unzip, Repeat, Zippy(..) )


type HVectorize :: Emptiability -> HKTable -> HKTable
data HVectorize emptiability table context where
  HVectorize :: table (H (VectorizeSpec emptiability context)) -> HVectorize emptiability table (H context)


instance HFunctor (HVectorize emptiability) where
  hmap f (HVectorize table) = HVectorize (f table)


type HVectorizeField :: Emptiability -> HKTable -> Context
data HVectorizeField emptiability table spec where
  HVectorizeField
    :: HField table ('Spec necessity nullability a)
    -> HVectorizeField emptiability table
       ('Spec necessity 'NonNullable (Array emptiability nullability a)
       )


instance (HTable table, KnownEmptiability emptiability) =>
  HTable (HVectorize emptiability table)
 where

  type HField (HVectorize emptiability table) =
    HVectorizeField emptiability table
  type HConstrainTable (HVectorize emptiability table) c =
    HConstrainTable table (VectorizeSpecC emptiability c)

  hfield (HVectorize table) (HVectorizeField field) =
    getVectorizeSpec (hfield table field)

  htabulate f = HVectorize $ htabulate $ \field -> case hfield hspecs field of
    SSpec {} -> VectorizeSpec (f (HVectorizeField field))
  htraverse f (HVectorize t) = HVectorize <$> htraverse (traverseVectorizeSpec f) t

  hdicts :: forall c. HConstrainTable table (VectorizeSpecC emptiability c) =>
    HVectorize emptiability table (H (Dict c))
  hdicts = HVectorize $ htabulate $ \field -> case hfield hspecs field of
    SSpec {} -> case hfield (hdicts @_ @(VectorizeSpecC emptiability c)) field of
      Dict -> VectorizeSpec Dict

  hspecs = HVectorize $ htabulate $ \field -> case hfield hspecs field of
    SSpec necessity nullability a -> VectorizeSpec
      ( SSpec necessity SNonNullable
          (arrayTypeInformation emptiabilitySing nullability a)
      )


type VectorizingSpec :: Type -> Type
type VectorizingSpec r = Emptiability -> (Spec -> r) -> Spec -> r


type VectorizeSpec :: VectorizingSpec Type
data VectorizeSpec emptiability context spec where
  VectorizeSpec ::
    { getVectorizeSpec :: context
      ( 'Spec necessity 'NonNullable (Array emptiability nullability a)
      )
    }
    -> VectorizeSpec emptiability context ('Spec necessity nullability a)


type VectorizeSpecC :: VectorizingSpec Constraint
class
  ( forall necessity nullability a.
    ( spec ~ 'Spec necessity nullability a => constraint
      ( 'Spec necessity 'NonNullable (Array emptiability nullability a)
      )
    )
  ) => VectorizeSpecC emptiability constraint spec
instance
  ( spec ~ 'Spec necessity nullability a
  , constraint
    ( 'Spec necessity 'NonNullable (Array emptiability nullability a)
    )
  ) => VectorizeSpecC emptiability constraint spec


traverseVectorizeSpec :: forall context context' emptiability spec m. Functor m
  => (forall x. context x -> m (context' x))
  -> VectorizeSpec emptiability context spec
  -> m (VectorizeSpec emptiability context' spec)
traverseVectorizeSpec f (VectorizeSpec a) = VectorizeSpec <$> f a


hvectorize :: (HTable t, Unzip f)
  => (forall necessity nullability a. ()
    => SSpec ('Spec necessity nullability a)
    -> f (context ('Spec necessity nullability a))
    -> context'
      ( 'Spec necessity 'NonNullable (Array emptiability nullability a)
      ))
  -> f (t (H context))
  -> HVectorize emptiability t (H context')
hvectorize vectorizer as = HVectorize $ htabulate $ \field ->
  case hfield hspecs field of
    spec@SSpec {} -> VectorizeSpec (vectorizer spec (fmap (`hfield` field) as))


hunvectorize :: (HTable t, Repeat f)
  => (forall necessity nullability a. ()
    => SSpec ('Spec necessity nullability a)
    -> context ('Spec necessity 'NonNullable (Array emptiability nullability a))
    -> f (context' ('Spec necessity nullability a)))
  -> HVectorize emptiability t (H context)
  -> f (t (H context'))
hunvectorize unvectorizer (HVectorize table) =
  getZippy $ htabulateA $ \field -> case hfield hspecs field of
    spec -> case hfield table field of
      VectorizeSpec a -> Zippy (unvectorizer spec a)


happend :: HTable t
  => (forall necessity nullability a. ()
    => SNullability nullability
    -> TypeInformation a
    -> context ('Spec necessity 'NonNullable (Array emptiability nullability a))
    -> context ('Spec necessity 'NonNullable (Array emptiability nullability a))
    -> context ('Spec necessity 'NonNullable (Array emptiability nullability a)))
  -> HVectorize emptiability t (H context)
  -> HVectorize emptiability t (H context)
  -> HVectorize emptiability t (H context)
happend append (HVectorize as) (HVectorize bs) = HVectorize $
  htabulate $ \field -> case (hfield as field, hfield bs field) of
    (VectorizeSpec a, VectorizeSpec b) -> case hfield hspecs field of
      SSpec _ nullability info -> VectorizeSpec $ append nullability info a b


hempty :: HTable t
  => (forall necessity nullability a. ()
    => SNullability nullability
    -> TypeInformation a
    -> context ('Spec necessity 'NonNullable (Array emptiability nullability a)))
  -> HVectorize emptiability t (H context)
hempty empty = HVectorize $ htabulate $ \field -> case hfield hspecs field of
  SSpec _ nullability info -> VectorizeSpec (empty nullability info)
