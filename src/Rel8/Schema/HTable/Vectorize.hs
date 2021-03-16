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
  , hrelabel
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.Kind.Blueprint
  ( Blueprint( Vector )
  , SBlueprint( SVector )
  , ToDBType
  )
import Rel8.Kind.Emptiability
  ( Emptiability, KnownEmptiability, emptiabilitySing
  )
import Rel8.Kind.Nullability
  ( Nullability( NonNullable )
  , SNullability( SNonNullable )
  )
import Rel8.Schema.Context.Label ( Labelable, labeler, unlabeler )
import Rel8.Schema.Dict ( Dict( Dict ) )
import Rel8.Schema.HTable
  ( HTable, HConstrainTable, HField
  , hfield, htabulate, htabulateA, htraverse, hdicts, hspecs
  )
import Rel8.Schema.HTable.Context ( H, HKTable )
import Rel8.Schema.Spec
  ( Context
  , Spec( Spec )
  , SSpec(..)
  )
import Rel8.Type ( TypeInformation )
import Rel8.Type.Array ( arrayTypeInformation )

-- semialign
import Data.Zip ( Unzip, Repeat, Zippy(..) )


type HVectorize :: Emptiability -> HKTable -> HKTable
data HVectorize emptiability table context where
  HVectorize :: table (H (VectorizeSpec emptiability context)) -> HVectorize emptiability table (H context)


type HVectorizeField :: Emptiability -> HKTable -> Context
data HVectorizeField emptiability table spec where
  HVectorizeField
    :: HField table ('Spec labels necessity nullability blueprint)
    -> HVectorizeField emptiability table
       ('Spec labels necessity 'NonNullable
          ('Vector emptiability nullability blueprint)
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
    SSpec labels necessity nullability blueprint typeInformation -> VectorizeSpec
      SSpec
        { slabels = labels
        , snecessity = necessity
        , snullability = SNonNullable
        , sblueprint = SVector emptiabilitySing nullability blueprint
        , stypeInformation =
            arrayTypeInformation emptiabilitySing nullability typeInformation
        }


type VectorizingSpec :: Type -> Type
type VectorizingSpec r = Emptiability -> (Spec -> r) -> Spec -> r


type VectorizeSpec :: VectorizingSpec Type
data VectorizeSpec emptiability context spec where
  VectorizeSpec ::
    { getVectorizeSpec :: context
      ( 'Spec labels necessity 'NonNullable
          ('Vector emptiability nullability blueprint)
      )
    }
    -> VectorizeSpec emptiability context
         ('Spec labels necessity nullability blueprint)


instance Labelable context => Labelable (VectorizeSpec emptiability context) where
  labeler (VectorizeSpec a) = VectorizeSpec (labeler a)
  unlabeler (VectorizeSpec a) = VectorizeSpec (unlabeler a)


type VectorizeSpecC :: VectorizingSpec Constraint
class
  ( forall labels necessity nullability blueprint.
    ( spec ~ 'Spec labels necessity nullability blueprint => constraint
      ( 'Spec labels necessity 'NonNullable
          ('Vector emptiability nullability blueprint)
      )
    )
  ) => VectorizeSpecC emptiability constraint spec
instance
  ( spec ~ 'Spec labels necessity nullability blueprint
  , constraint
    ( 'Spec labels necessity 'NonNullable
        ('Vector emptiability nullability blueprint)
    )
  ) => VectorizeSpecC emptiability constraint spec


traverseVectorizeSpec :: forall context context' emptiability spec m. Functor m
  => (forall x. context x -> m (context' x))
  -> VectorizeSpec emptiability context spec
  -> m (VectorizeSpec emptiability context' spec)
traverseVectorizeSpec f (VectorizeSpec a) = VectorizeSpec <$> f a


hvectorize :: (HTable t, Unzip f)
  => (forall labels necessity nullability blueprint. ()
    => SSpec ('Spec labels necessity nullability blueprint)
    -> f (context ('Spec labels necessity nullability blueprint))
    -> context'
      ( 'Spec labels necessity 'NonNullable
          ('Vector emptiability nullability blueprint)
      ))
  -> f (t (H context))
  -> HVectorize emptiability t (H context')
hvectorize vectorizer as = HVectorize $ htabulate $ \field ->
  case hfield hspecs field of
    spec@SSpec {} -> VectorizeSpec (vectorizer spec (fmap (`hfield` field) as))


hunvectorize :: (HTable t, Repeat f)
  => (forall labels necessity nullability blueprint. ()
    => SSpec ('Spec labels necessity nullability blueprint)
    -> context
         ( 'Spec labels necessity 'NonNullable
             ('Vector emptiability nullability blueprint)
         )
    -> f (context' ('Spec labels necessity nullability blueprint)))
  -> HVectorize emptiability t (H context)
  -> f (t (H context'))
hunvectorize unvectorizer (HVectorize table) =
  getZippy $ htabulateA $ \field -> case hfield hspecs field of
    spec -> case hfield table field of
      VectorizeSpec a -> Zippy (unvectorizer spec a)


happend :: HTable t =>
  ( forall labels necessity nullability blueprint. ()
    => SNullability nullability
    -> TypeInformation (ToDBType blueprint)
    -> context
         ( 'Spec labels necessity 'NonNullable
             ('Vector emptiability nullability blueprint)
         )
     -> context
         ( 'Spec labels necessity 'NonNullable
             ('Vector emptiability nullability blueprint)
         )
    -> context
         ( 'Spec labels necessity 'NonNullable
            ( 'Vector emptiability nullability blueprint)
         )
  )
  -> HVectorize emptiability t (H context)
  -> HVectorize emptiability t (H context)
  -> HVectorize emptiability t (H context)
happend append (HVectorize as) (HVectorize bs) = HVectorize $
  htabulate $ \field -> case (hfield as field, hfield bs field) of
    (VectorizeSpec a, VectorizeSpec b) -> case hfield hspecs field of
      SSpec _ _ nullability _ info -> VectorizeSpec $ append nullability info a b


hempty :: HTable t =>
  ( forall labels necessity nullability blueprint. ()
    => SNullability nullability
    -> TypeInformation (ToDBType blueprint)
    -> context
         ('Spec labels necessity 'NonNullable
           ( 'Vector emptiability nullability blueprint
           )
         )
  )
  -> HVectorize emptiability t (H context)
hempty empty = HVectorize $ htabulate $ \field -> case hfield hspecs field of
  SSpec _ _ nullability _ info -> VectorizeSpec (empty nullability info)


hrelabel :: Labelable context
  => (forall ctx. Labelable ctx => t (H ctx) -> u (H ctx))
  -> HVectorize emptiability t (H context)
  -> HVectorize emptiability u (H context)
hrelabel f (HVectorize table) = HVectorize (f table)
