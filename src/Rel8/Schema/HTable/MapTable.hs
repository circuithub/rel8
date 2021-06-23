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
import Data.Kind ( Constraint )
import Prelude ( ($), (.), (<$>), fmap )

-- rel8
import Rel8.FCF
import Rel8.Schema.HTable
import Rel8.Schema.Spec ( Spec, SSpec )
import qualified Rel8.Schema.Kind as K
import Rel8.Schema.Dict ( Dict( Dict ) )


type HMapTable :: (Spec -> Exp Spec) -> K.HTable -> K.HTable
newtype HMapTable f t context = HMapTable
  { unHMapTable :: t (Precompose f context)
  }


type Precompose :: (Spec -> Exp Spec) -> K.Context -> K.Context
newtype Precompose f g x = Precompose
  { precomposed :: g (Eval (f x))
  }


type HMapTableField :: (Spec -> Exp Spec) -> K.HTable -> K.Context
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


type MapSpec :: (Spec -> Exp Spec) -> Constraint
class MapSpec f where
  mapInfo :: SSpec x -> SSpec (Eval (f x))


type ComposeConstraint :: (Spec -> Exp Spec) -> (Spec -> Constraint) -> Spec -> Constraint
class c (Eval (f a)) => ComposeConstraint f c a
instance c (Eval (f a)) => ComposeConstraint f c a


hproject :: (forall ctx. t ctx -> u ctx) -> HMapTable f t context -> HMapTable f u context
hproject f (HMapTable t) = HMapTable (f t)
