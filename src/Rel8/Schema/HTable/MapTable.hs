{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}

module Rel8.Schema.HTable.MapTable
  ( HMapTable(..)
  , MapSpec(..)
  , Precompose(..)
  , HMapTableField(..)
  , hproject
  )
where

-- base
import Data.Kind ( Constraint, Type )
import Prelude

-- rel8
import Rel8.FCF ( Exp, Eval )
import Rel8.Schema.HTable
  ( HTable, HConstrainTable, HField
  , hfield, htabulate, htraverse, hdicts, hspecs
  )
import Rel8.Schema.Spec ( Spec )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Dict ( Dict( Dict ) )


type HMapTable :: (Type -> Exp Type) -> K.HTable -> K.HTable
newtype HMapTable f t context = HMapTable
  { unHMapTable :: t (Precompose f context)
  }


type Precompose :: (Type -> Exp Type) -> K.Context -> K.Context
newtype Precompose f g x = Precompose
  { precomposed :: g (Eval (f x))
  }


type HMapTableField :: (Type -> Exp Type) -> K.HTable -> K.Context
data HMapTableField f t x where
  HMapTableField :: HField t a -> HMapTableField f t (Eval (f a))


instance (HTable t, MapSpec f) => HTable (HMapTable f t) where
  type HField (HMapTable f t) = 
    HMapTableField f t

  type HConstrainTable (HMapTable f t) c =
    HConstrainTable t (ComposeConstraint f c)

  hfield (HMapTable x) (HMapTableField i) = 
    precomposed (hfield x i) 

  htabulate f = 
    HMapTable $ htabulate (Precompose . f . HMapTableField)

  htraverse f (HMapTable x) = 
    HMapTable <$> htraverse (fmap Precompose . f . precomposed) x
  {-# INLINABLE htraverse #-}

  hdicts :: forall c. HConstrainTable (HMapTable f t) c => HMapTable f t (Dict c)
  hdicts = 
    htabulate \(HMapTableField j) ->
      case hfield (hdicts @_ @(ComposeConstraint f c)) j of
        Dict -> Dict

  hspecs = 
    HMapTable $ htabulate $ Precompose . mapInfo @f . hfield hspecs
  {-# INLINABLE hspecs #-}


type MapSpec :: (Type -> Exp Type) -> Constraint
class MapSpec f where
  mapInfo :: Spec x -> Spec (Eval (f x))


type ComposeConstraint :: (Type -> Exp Type) -> (Type -> Constraint) -> Type -> Constraint
class c (Eval (f a)) => ComposeConstraint f c a
instance c (Eval (f a)) => ComposeConstraint f c a


hproject :: ()
  => (forall ctx. t ctx -> t' ctx)
  -> HMapTable f t context -> HMapTable f t' context
hproject f (HMapTable a) = HMapTable (f a)
